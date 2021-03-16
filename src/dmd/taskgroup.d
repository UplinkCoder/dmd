module dmd.taskgroup;

import core.thread.fiber;
import core.stdc.stdio;

import core.thread.threadbase;
import core.thread.osthread;


Thread[4] unorderedBackgroudThreads;
TaskQueue[4] unorderedBackgroudQueue;

import core.sys.posix.pthread;

align(16) struct TaskQueue
{
    shared align(16) uint queueLock;
    shared align(16) uint currentTickit;

    // these can only be touched when locked
    __gshared Task*[64] tasks;
    __gshared uint n_queue_full;

    void addTaskToQueue(Task* task, uint queueID)
    {
        assert(task.queueID == 0, "trying task which is already in a queue");
        assert(queueID != 0, "Queue 0 means unowned, therefore can't be used as valid QueueID");
        import core.atomic /*: atomicFence, cas, atomicFetchAdd*/;

        auto tickit = atomicFetchAdd(currentTickit, 1);
        if (tickit == 0)
        {
            tickit = 1;
        }

        // lock is presumably  unowned
        static immutable string __mmPause = "asm nothrow pure { rep; nop; }";
    Lspin:

        while (queueLock != 0 || n_queue_full == tasks.length) { mixin(__mmPause); }  // spin
        if (atomicLoad(queueLock) == 0)
        {
            // try aquiring it
            if (cas(&queueLock, 0, tickit))
            {
                // we have the lock
                if (n_queue_full < tasks.length)
                {
                    tasks[n_queue_full++] = task;
                    task.queueID = queueID;
                    atomicFence();
                }
                atomicStore(queueLock, 0); // unlock
            }
            else
                goto Lspin;
        }
    }
}

void initBackgroundThreads()
{
    foreach(i, ref t; unorderedBackgroudThreads)
    {
        t = new Thread(() {
                for(;;)
                {

                }
        });
    }
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

void dispatchToBackgroudThread(Task* task)
{
    static shared size_t queueCounter = 0;
    import core.atomic;
    auto queueID = atomicFetchAdd(queueCounter, 1) % unorderedBackgroudQueue.length;
    
}

struct Task
{
    void* delegate (void*) fn;
    void* taskData;
    Task*[] children;
    size_t n_children_completed;

    size_t queueID;

    TaskFiber currentFiber;
    bool hasCompleted;

    //debug (task)
    //{
        OriginInformation originInfo;
    //}
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
            currentTask.hasCompleted = true;
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
            dispatchToBackgroudThread(task);
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
            if (!task.hasCompleted)
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
                if (!task.hasCompleted)
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
        //import dmd.root.rootobject;
        //printf("running for %s\n", (cast(RootObject)currentTask.taskData).toChars());

        //TODO instead of new'ing a TaskFiber use a pool of them.
        if (!currentTask.currentFiber)
            currentTask.currentFiber = new TaskFiber(currentTask);

        currentTask.currentFiber.call();
        if (currentTask.hasCompleted || currentTask.currentFiber.hasCompleted)
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

    assert(task1_completed);
    assert(task2_completed);
}
