module dmd.taskgroup;

import core.thread.fiber;
import core.stdc.stdio;

import core.thread.threadbase;
import core.thread.osthread;


enum n_threads = 1;

Thread[n_threads] unorderedBackgroundThreads;
shared TaskQueue[n_threads] unorderedBackgroundQueue;
shared bool[n_threads] killThread;

import core.sys.posix.pthread;

import core.atomic;

extern (C) struct TaskQueue
{
    shared align(16) uint nextTicket;
    shared align(16) uint currentlyServing;

    // these can only be touched when locked
    shared Task*[1] tasks;
    shared uint insert_pos;
    shared uint currently_executing;

    void addTaskToQueue(Task* task, uint queueID) shared
    {
        assert(queueID != 0, "A zero queueID is invalid (0 means unowned)");
        const myTicket = atomicFetchAdd(nextTicket, 1);
        
        scope (exit)
            atomicFetchAdd(currentlyServing, 1);

        printf("currentlyServing: %d, nextTickit: %d\n", currentlyServing, nextTicket);

        for (;;)
        {
            if (insert_pos == tasks.length || atomicLoad(currentlyServing) != myTicket)
                continue;
            printf(" got the lock \n");
            /* do locked stuff here ... */
            if (insert_pos < tasks.length)
            {
                task.queueID = queueID;
                tasks[atomicFetchAdd(insert_pos, 1)] = cast(shared)task;
                atomicFence();
                break;
            }
        }
    }
}

extern (C) void breakpoint() {}

void initBackgroundThreads()
{
    int i;
    foreach(ref t; unorderedBackgroundThreads)
    {
        t = new Thread(() {
                const uint thread_idx = cast(uint)i++;
                printf("thread_proc start thread_id: %d\n", thread_idx + 1);
                auto myQueue = &unorderedBackgroundQueue[thread_idx];

                while(true && !killThread[thread_idx])
                {
                    import core.atomic;
                    while(atomicLoad(myQueue.insert_pos) != 0)
                    {
                        const myTicket = atomicFetchAdd(myQueue.nextTicket, 1);
                        
                        scope (exit)
                            atomicFetchAdd(myQueue.currentlyServing, 1);
                        
                        for (;;)
                        {
                            if (atomicLoad(myQueue.currentlyServing) != myTicket)
                                continue;

                            Task task = cast(Task) *myQueue.tasks[atomicFetchAdd(myQueue.currently_executing, 1) % myQueue.tasks.length];
                            printf("pulled task with queue_id %d and thread_id is: %d and myQueue is: %p\n", task.queueID, thread_idx + 1, myQueue);
                            assert(task.queueID == thread_idx + 1);
                            task.assignFiber();
                            task.callFiber();
                            if (task.hasCompleted())
                            {
                                atomicFetchSub(myQueue.insert_pos, 1);
                            }
                        }
                    }
                    if (i == 3) breakpoint();
                    // sleep for a microsecond if empty
                    {
                        //import core.time;
                        //Thread.sleep(dur!"usecs"(1));
                    }
                }

        });
        t.start();
    }

}

void killBackgroundThreads()
{
    foreach(i; 0 .. unorderedBackgroundThreads.length)
        killThread[i] = true;
}

size_t align16(const size_t n) pure
{
    pragma(inline, true);
    return ((n + 15) & ~15);
}

struct OriginInformation
{
    string filename;
    uint line;
    Task* originator;
}

void dispatchToBackgroundThread(Task* task)
{
    static shared uint queueCounter = 0;
    uint queueID = atomicFetchAdd(queueCounter, 1) % cast(uint)(unorderedBackgroundQueue.length);
    unorderedBackgroundQueue[queueID].addTaskToQueue(task, queueID + 1);
}

struct Task
{
    void* delegate (void*) fn;
    void* taskData;
    Task*[] children;
    size_t n_children_completed;

    uint queueID;
    bool isBackgroundTask;

    TaskFiber currentFiber;
    bool hasCompleted_;


    bool hasCompleted()
    {
        return (hasCompleted_ || (currentFiber && currentFiber.hasCompleted()));
    }
    //debug (task)
    //{
        OriginInformation originInfo;
    //}

    void assignFiber()
    {        
        //TODO instead of new'ing a TaskFiber use a pool of them.
        if (!hasCompleted_ && !currentFiber)
            currentFiber = new TaskFiber(&this);
    }

    void callFiber()
    {
        if (!hasCompleted_ && currentFiber && currentFiber.state() != currentFiber.State.EXEC)
            currentFiber.call();
    }
}

class TaskFiber : Fiber
{
    Task* currentTask;

    this(Task* currentTask)
    {
        super(&doTask, ushort.max);
        // use large stack of ushort.max
        this.currentTask = currentTask;
        currentTask.currentFiber = this;
    }

    void doTask()
    {
        assert(state() != State.TERM, "Attempting to start a finished task");
        currentTask.fn(currentTask.taskData);
    }

    bool hasCompleted()
    {
        if (state() == State.TERM)
        {
            currentTask.currentFiber = null;
            currentTask.hasCompleted_ = true;
            reset();
            return true;
        }
        else
            return false;
    }
}

struct TaskGroup
{
    string name;
    size_t n_used;
    size_t n_completed;

    Task[] tasks;
 
    static Task* getCurrentTask()
    {
        TaskFiber fiber = cast(TaskFiber) Fiber.getThis();
        if (!fiber)
            return null;
        else
            return fiber.currentTask;
    }

    private void allocate_tasks(size_t n)
    {
        import dmd.root.rmem;

        assert(tasks.length < n);
        n = align16(n);
        const n_bytes = n * Task.sizeof;
        auto ptr = cast(Task*)(tasks.ptr ? Mem.xrealloc_noscan(tasks.ptr, n_bytes) : Mem.xmalloc_noscan(n_bytes));
        tasks = ptr[0 .. n];
    }
    
    Task* addTask(void* delegate (void*) fn, void* taskData, bool background_task = false,
        Task* originator = getCurrentTask(), size_t line = __LINE__, string file = __FILE__)
    {
        if (tasks.length <= n_used)
        {
            size_t n_tasks;

            n_tasks = tasks.length == 0 ?
                1 :
                cast(size_t) (tasks.length * 1.2);

            allocate_tasks(n_tasks);
        }

        auto task = &tasks[n_used++]; 
        *task = Task(fn, taskData);
        if (originator)
            originator.children ~= task;

        if (background_task)
        {
            task.isBackgroundTask = true;
            dispatchToBackgroundThread(task);
        }

        //debug (task)
        {
            task.originInfo = OriginInformation(file, cast(int)line, originator);
        }

        debug (immedaiteTaskCompletion)
        {
            task.currentFiber = new TaskFiber(task);
            task.currentFiber.call();
            if(!(task.hasCompleted || task.currentFiber.hasCompleted))
            {
                import dmd.root.rootobject;
                fprintf(stderr, "[TaskGroup: %s] No immediate task completion possible for '%s'\n", this.name.ptr, (cast(RootObject)task.taskData).toChars());
            }
            else
            {
                n_completed++;
            }
        }

        return task;
    }

    Task* findTask(void *taskData)
    {
        foreach(ref task;tasks)
        {
            if (task.taskData == taskData)
                return &task;
        }
        return null;
    }

    this(string name, size_t n_allocated = 0)
    {
        this.name = name;
        
        if (n_allocated)
            allocate_tasks(n_allocated);
    }
    
    void runTask ()
    {
        if (n_completed == n_used)
            return ;
        // if task group is done, don't try to run anything.

        Task* parent;
        Task* currentTask;

        foreach(ref task; tasks)
        {
            if (!task.hasCompleted())
            {
                currentTask = &task;
                break;
            }
        }

        if (currentTask is null)
        {
            char[255] assertMessageBuffer;
            size_t messageLength = sprintf(assertMessageBuffer.ptr, 
                "All tasks seem to have completed but n_used is %zu and n_completed is %zu which means that %ld tasks have not registered completion",
                n_used, n_completed, (cast(long)(n_used - n_completed))
            );
            assert(currentTask, cast(string)assertMessageBuffer[0 .. messageLength]);
        }

        while (currentTask.children.length)
        {
            parent = currentTask;
            foreach(ref task; currentTask.children)
            {
                if (!task.hasCompleted())
                {
                    currentTask = task;
                    break;
                }
            }

            if (currentTask is null)
            {
                char[255] assertMessageBuffer;
                size_t messageLength = sprintf(assertMessageBuffer.ptr, 
                    "All tasks seem to have completed but n_used is %zu and n_completed is %zu which means that %ld tasks have not registered completion",
                    n_used, n_completed, (cast(ptrdiff_t)(parent.children.length - parent.n_children_completed))
                );
                assert(currentTask, cast(string)assertMessageBuffer[0 .. messageLength]);
            }
        }
        import dmd.root.rootobject;
        if (auto ro = cast(RootObject) currentTask.taskData)
        {
            // printf("running for %s\n", ro.toChars());
        }

        if (!currentTask.isBackgroundTask)
        {
            currentTask.assignFiber();
            currentTask.callFiber();
        }


        if (currentTask.hasCompleted())
        {
            if (parent)
            {
                parent.n_children_completed++;
                parent.children = parent.children[1 .. $];
            }
            else
            {
                n_completed++;
            }
        }
        //TODO have this actually use fibers!
    }
    
    void awaitCompletionOfAllTasks()
    {
        runTask();
        while(n_completed < n_used) { runTask(); }
    }
}

string taskGraph(Task* task, Task* parent = null, int indent = 0)
{

    static char[] formatPtr(Task* t)
    {
        char[32] formatBuffer; // only used for toHex conversion of ptr

        string result;

        auto len = sprintf(formatBuffer.ptr, "\"%p\"", t);
        return formatBuffer[0 .. len].dup;
    }

    string result;
    if (parent is null)
    {
        result ~= "digraph Tasks {\n";
    }

    foreach(_; 0 .. indent++)
    {
        result ~= "\t";
    }

    result ~= formatPtr(parent) ~ " -> " ~ formatPtr(task) ~ "\n";

    foreach(ref c;task.children)
    {
        result ~= taskGraph(c, task, indent);
        //result ~= formatPtr(task) ~ " > " ~ formatPtr(c);
    }

    if (parent is null)
    {
        result ~= "\n}";
    }

    return result;
}

@("Test Originator")
unittest
{
    bool task1_completed = 0;
    bool task2_completed = 0;

    size_t line = __LINE__;
    TaskGroup tg = TaskGroup("tg1", 3);
    tg.addTask((void* x) { // line + 2
        auto task = tg.addTask((void* x) { // line + 3
            task2_completed = true;
            return null;
        }, x);
        auto task2 = tg.addTask((void* x) { // line + 7
            task2_completed = true;
            return null;
        }, x);
        
        assert(task.originInfo.line == line + 3);
        assert(task2.originInfo.line == line + 7);
        assert(task.originInfo.originator.originInfo.line == line + 2);
        task1_completed = true;
        return x;
    }, null);

    tg.awaitCompletionOfAllTasks();

    import std.stdio;
    writeln(taskGraph(&tg.tasks[0]));

    assert(task1_completed);
    assert(task2_completed);
}
