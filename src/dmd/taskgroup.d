
module dmd.taskgroup;

import core.thread.fiber;
import core.stdc.stdio;
import core.thread.osthread;
import core.atomic;


enum n_threads = 4;

Thread[n_threads] unorderedBackgroundThreads;
shared TaskQueue[n_threads] unorderedBackgroundQueue;
shared bool[n_threads] killThread;

import core.sys.posix.pthread;


// debug = ImmediateTaskCompletion;

extern (C) struct TaskQueue
{
    shared align(16) uint nextTicket;
    shared align(16) uint currentlyServing;

    // these can only be touched when locked
    shared Task*[1] tasks;
    align(16) shared int insert_pos;
    align(16) shared int exec_pos;

    void addTaskToQueue(shared Task* task, uint queueID) shared
    {
        assert(queueID != 0, "A zero queueID is invalid (0 means unowned)");
        // while we have no room to insert anything there's no point in drawing a ticket
        while(atomicLoad(insert_pos) == tasks.length)
	{
          // possibly pause ?            
	}
        auto myTicket = atomicFetchAdd(nextTicket, 1);
        scope (exit)
            atomicFetchAdd(currentlyServing, 1);
        

        // printf("currentlyServing: %d, nextTickit: %d\n", currentlyServing, nextTicket);

        for (;;)
        {
            if (atomicLoad(currentlyServing) != myTicket)
            {
                // printf("could not get lock for enqueue {insert_pos = %d, currentlyServing = %d, myTicket = %d}\n", insert_pos, currentlyServing, myTicket);
                continue;
            }
            // printf(" got the lock for %d\n", myTicket);

            if (atomicLoad(insert_pos)  == tasks.length)
            {
               // printf("TaskQueue is full ... retrying\n");
               // if the task queue is full we need to relinquish our ticket
               atomicFetchAdd(currentlyServing, 1);
               // and draw a new one
               myTicket = atomicFetchAdd(nextTicket, 1);
               continue;
            }


            /* do locked stuff here ... */
            if (atomicLoad(insert_pos) < tasks.length)
            {
                task.queueID = queueID;
                tasks[atomicFetchAdd(insert_pos, 1)] = cast(shared)task;
                atomicFence();
                break;
            }
            else
                assert(0, "we should have made sure that the queue has room");
        }
    }
}

extern (C) void breakpoint() {}

shared uint thread_counter;
void initBackgroundThreads()
{
    foreach(ref t; unorderedBackgroundThreads)
    {
        t = new Thread(() {
                const uint thread_idx = atomicFetchAdd(thread_counter, 1);
                // printf("thread_proc start thread_id: %d\n", thread_idx + 1);
                auto myQueue = &unorderedBackgroundQueue[thread_idx];

                while(true && !killThread[thread_idx])
                {
                    import core.atomic;
                    while(atomicLoad(myQueue.insert_pos) > 0)
                    {
                        const myTicket = atomicFetchAdd(myQueue.nextTicket, 1);
                        
                        scope (exit)
                            atomicFetchAdd(myQueue.currentlyServing, 1);
                        
                        for (;;)
                        {
                            if (atomicLoad(myQueue.currentlyServing) != myTicket)
                                continue;
                            // printf("TaskProc aquired lock using ticket: %d\n", myTicket);
                            // now that we have aquired the lock we need to check if the task we wanted to do wasn't already done
                            if (atomicLoad(myQueue.insert_pos) == 0)
                            {
                                // the queue is empty and we need to relinquish the lock
                                break;
                            }
                            auto task = myQueue.tasks[atomicFetchAdd(myQueue.exec_pos, 1) % myQueue.tasks.length];
                            assert(task.queueID);
                            // printf("pulled task with queue_id %d and thread_id is: %d and myQueue is: %p\n", task.queueID, thread_idx + 1, myQueue);
                            assert(task.queueID == thread_idx + 1);
                            import dmd.root.rootobject;

                            if (auto ro = cast(RootObject) task.taskData)
                            {
                                // printf("[BackgroundThread] running for %s\n", ro.toChars());
                            }

                            task.assignFiber();
                            task.callFiber();
                            if (task.hasCompleted())
                            {
                                // printf("task %p has completed myQueue.insert_pos: %d\n", task, myQueue.insert_pos);
                                atomicFetchSub(myQueue.insert_pos, 1);
                                // printf("myQueue.insert_pos: %d\n", myQueue.insert_pos);
                            }
                            break;
                        }
                    }
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
    shared Task* originator;
}

void dispatchToBackgroundThread(shared Task* task)
{
    static shared uint queueCounter = 0;
    uint queueID = atomicFetchAdd(queueCounter, 1) % cast(uint)(unorderedBackgroundQueue.length);
    unorderedBackgroundQueue[queueID].addTaskToQueue(task, queueID + 1);
}

shared struct Task
{
    shared (void*) delegate (shared void*) fn;
    shared (void*) taskData;
    TaskGroup* taskGroup;

    shared Task*[] children;
    shared size_t n_children_completed;

    uint queueID;
    bool isBackgroundTask;

    shared TaskFiber currentFiber;
    align(16) shared bool hasCompleted_;
    align(16) shared bool hasFiber;
    align(16) shared bool fiberIsExecuting;
    align(16) shared uint originatorAppendLock;
    align(16) shared uint readers;


    bool hasCompleted(string file = __FILE__, int line = __LINE__) shared
    {
        // printf("[%s:+%d]asking has Completed \n", file.ptr, line);
        atomicFetchAdd(readers, 1);
        scope (exit)
            atomicFetchSub(readers, 1);

        return (atomicLoad(hasCompleted_) || (atomicLoad(hasFiber) && !fiberIsExecuting && currentFiber.hasCompleted()));
    }
    //debug (task)
    //{
        OriginInformation originInfo;
    //}

    void assignFiber() shared
    {
        assert(fn !is null);
        //TODO instead of new'ing a TaskFiber use a pool of them.
        if (!atomicLoad(hasCompleted_) && !hasFiber)
        {
            if (cas(&hasFiber, false, true))
            {
                currentFiber = cast(shared)new TaskFiber(cast(Task*)&this);
            }
        }
    }

    void callFiber() shared
    {
        // a TaskFiber may only be executed by the thread that created it
        // therefore it cannot happen that a task gets completed by another thread.
        assert(hasFiber && !hasCompleted_);
        {
            auto unshared_fiber = cast()currentFiber;
            if (unshared_fiber.state() != unshared_fiber.State.EXEC &&
                cas(cast()&fiberIsExecuting, false, true))
            {
                (cast()currentFiber).call();
                atomicFence();
                fiberIsExecuting = false;
            }
        }
    }
}

class TaskFiber : Fiber
{
    Task* currentTask;
    align(16) shared bool hasTask;

    this(Task* currentTask)
    {
        assert(currentTask.hasFiber && currentTask.currentFiber is null);
        super(&doTask, ushort.max);
        // use large stack of ushort.max
        this.currentTask = currentTask;
        currentTask.currentFiber = cast(shared)this;
        hasTask = true;
    }

    void doTask()
    {
        assert(state() != State.TERM, "Attempting to start a finished task");
        currentTask.fn(currentTask.taskData);
    }

    bool hasCompleted() shared
    {
        if (!atomicLoad(hasTask))
        {
            // if a fiber has no task it must have completed at a previous point;
            // we assume that the request is stale and return true;
            return true;
        }
        assert(currentTask && currentTask.hasFiber);
        auto state = (cast()this).state();
        if (state == State.TERM)
        {
            if (cas(&hasTask, true, false))
            {
                if (cas(&currentTask.hasCompleted_, false, true))
                {
                    atomicFetchAdd(currentTask.taskGroup.n_completed, 1);
                Lloop:
                    while(currentTask.readers > 1) {}
                    if (!cas(&currentTask.readers, 1, 1))
                        goto Lloop;

                    assert(currentTask.hasFiber);
                    atomicStore(currentTask.hasFiber, false);
                    currentTask.currentFiber = null;
                    //currentTask = null;
                    atomicStore(hasTask, false);
                    (cast()this).reset();
                }
            }
            return true;
        }
        else
            return false;
    }
}

enum TaskGroupFlags
{
   None = 0,
   ImmediateTaskCompletion = 1 >> 0,
}

enum DefaultTaskGroupFlags = TaskGroupFlags.None | TaskGroupFlags.ImmediateTaskCompletion;

struct TaskGroup
{
    string name;
    TaskGroupFlags flags;

    shared size_t n_used;
    shared size_t n_completed;

    shared Task[] tasks;
 
    static shared(Task)* getCurrentTask()
    {
        TaskFiber fiber = cast(TaskFiber) Fiber.getThis();
        if (!fiber)
            return null;
        else
            return cast(shared)fiber.currentTask;
    }

    private void allocate_tasks(size_t n) shared
    {
        import dmd.root.rmem;

        assert(tasks.length < n);
        n = align16(n);
        const n_bytes = n * Task.sizeof;
        auto ptr = cast(shared Task*)(tasks.ptr ? Mem.xrealloc_noscan(cast(void*)tasks.ptr, n_bytes) : Mem.xmalloc_noscan(n_bytes));
        tasks = ptr[0 .. n];
    }
    
    shared(Task)* addTask(shared(void*) delegate (shared void*) fn, shared void* taskData, bool background_task = false,
        shared Task* originator = getCurrentTask(), size_t line = __LINE__, string file = __FILE__) shared
    {
        if (tasks.length <= n_used)
        {
            shared size_t n_tasks;

            n_tasks = tasks.length == 0 ?
                1 :
                cast(size_t) (tasks.length * 1.2);

            allocate_tasks(n_tasks);
        }

        shared task = &tasks[atomicFetchAdd(n_used, 1)]; 
        *task = Task(fn, taskData, &this);
        if (originator) // technically we need to take a lock here!
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

        if (flags & TaskGroupFlags.ImmediateTaskCompletion)
        {
            task.assignFiber();
            task.callFiber();
            if(!(task.hasCompleted || task.currentFiber.hasCompleted()))
            {
                import dmd.root.rootobject;
                fprintf(stderr, "[TaskGroup: %s] No immediate task completion possible for '%s'\n", this.name.ptr, (cast(RootObject)task.taskData).toChars());
            }
            else
            {
                atomicFetchAdd(n_completed, 1);
            }
        }

        return task;
    }

    shared(Task)* findTask(void *taskData)
    {
        foreach(ref task;tasks)
        {
            if (task.taskData == taskData)
                return &task;
        }
        return null;
    }

    this(string name, size_t n_allocated = 0, TaskGroupFlags flags = DefaultTaskGroupFlags)
    {
        this.name = name;
        this.flags = flags;
        if (n_allocated)
            (cast(shared)(this)).allocate_tasks(n_allocated);
    }

    this(string name, TaskGroupFlags flags = DefaultTaskGroupFlags, size_t n_allocated = 0)
    {
        this.name = name;
        this.flags = flags;

        if (n_allocated)
            (cast(shared)(this)).allocate_tasks(n_allocated);
    }
    
    void runTask () shared
    {
        if (n_completed == n_used)
            return ;
        // if task group is done, don't try to run anything.

        shared Task* parent;
        shared Task* currentTask;

        foreach(ref task; tasks[0 .. n_used])
        {
            if (!task.isBackgroundTask && !atomicLoad(task.hasCompleted_) && !task.hasCompleted())
            {
                currentTask = &task;
                break;
            }
        }

        if (currentTask is null)
        {
            char[255] assertMessageBuffer = '0';
            size_t messageLength = sprintf(assertMessageBuffer.ptr, 
                "All tasks seem to have completed but n_used is %zu and n_completed is %zu which means that %ld tasks have not registered completion",
                n_used, n_completed, (cast(long)(n_used - n_completed))
            );
            assert(currentTask, cast(string)assertMessageBuffer[0 .. messageLength]);
        }

        if (currentTask !is null)
        {
            while (currentTask.children.length)
            {
                parent = currentTask;
                foreach(ref task; currentTask.children)
                {
                    if (!task.isBackgroundTask && !atomicLoad(task.hasCompleted_) && !task.hasCompleted())
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


            if (!currentTask.isBackgroundTask && currentTask.fn)
            {
                import dmd.root.rootobject;
                if (auto ro = cast(RootObject) currentTask.taskData)
                {
                    // printf("running for %s\n", ro.toChars());
                }

                currentTask.assignFiber();
                currentTask.callFiber();

                if (parent)
                {
                    atomicFetchAdd(parent.n_children_completed, 1);
                    // we need to get a lock for modifiung parent.children!
                    // TODO FIXME LOCKING
                    parent.children = parent.children[1 .. $];
                    // TODO FIXME LOCKING
                }
                else
                {
                    atomicFetchAdd(n_completed, 1);
                }
            }
        }
    }
    
    void awaitCompletionOfAllTasks() shared
    {
        while(n_completed < n_used) { runTask(); }
/+
        foreach(ref task;tasks[0 .. n_used])
        {
            import std.stdio;
            writeln(taskGraph(&task));
        }
+/
    }
}

string taskGraph(Task* task, Task* parent = null, int indent = 0)
{
    import dmd.root.rootobject;
    static char[] formatTask(Task* t)
    {
        import core.stdc.stdlib;
        char[] formatBuffer = cast(char[])malloc(4096 * 32)[0  .. 2096 * 32];
        scope(exit) free(formatBuffer.ptr);

        string result;

        if (!t)
            return cast(char[])"\"null, (TheRoot)\"";

        auto ro = cast(RootObject)t.taskData;
        auto tg = t.taskGroup;
        auto len = sprintf(formatBuffer.ptr, "\"%p {%s} (%s)\"", t, (tg && tg.name ? tg.name.ptr : null), (ro ? ro.toChars() : ""));
        return formatBuffer[0 .. len].dup;
    }

    string result;
    if (parent is null)
    {
        result ~= "digraph \"" ~ task.taskGroup.name ~ "\" {\n";
    }

    foreach(_; 0 .. indent++)
    {
        result ~= "\t";
    }

    result ~= formatTask(parent) ~ " -> " ~ formatTask(task) ~ "\n";

    foreach(ref c;task.children)
    {
        result ~= taskGraph(cast(Task*)c, task, indent);
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
    shared TaskGroup tg = TaskGroup("tg1", 3);
    tg.addTask((shared void* x) { // line + 2
        auto task = tg.addTask((shared void* x) { // line + 3
            task2_completed = true;
            return null;
        }, x);
        auto task2 = tg.addTask((shared void* x) { // line + 7
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
