
module dmd.taskgroup;

import core.thread.myFiber;
import core.stdc.stdio;
import core.thread.osthread;
import core.atomic;


enum n_threads = 4;

Thread[n_threads] unorderedBackgroundThreads;
shared TaskQueue[n_threads] unorderedBackgroundQueue;
__gshared FiberPool*[n_threads] unorderedBackgroundFiberPools;
shared bool[n_threads] killThread;

version (MULTITHREAD)
{
    enum MULTITHREAD = true;
}
else
{
    enum MULTITHREAD = false;
}

void initalize()
{
    static if (MULTITHREAD)
    {

    }
}

struct FiberPool
{
    TaskFiber[64] fibers = null;
    void* fiberPoolStorage = null;

    uint freeBitfield = ~0;

    static immutable INVALID_FIBER_IDX = uint.max;

    uint nextFree()
    {
        pragma(inline, true);
        import core.bitop : bsf;
        return freeBitfield ? bsf(freeBitfield)  : INVALID_FIBER_IDX;
    }

    uint n_free()
    {
        pragma(inline, true);
        import core.bitop : popcnt;
        return popcnt(freeBitfield);
    }

    uint n_used()
    {
        pragma(inline, true);
        import core.bitop : popcnt;
        return cast(uint) (fibers.length - popcnt(freeBitfield));
    }

    bool isFull()
    {
        pragma(inline, true);
        return !n_free();
    }

    void initFiberPool()
    {
        import core.stdc.stdlib;
        version (none)
        {
            static immutable aligned_size = align16(__traits(classInstanceSize, TaskFiber));
            this.fiberPoolStorage = malloc(aligned_size * fibers.length);
            pool.fibers = (cast(TaskFiber*)pool.fiberPoolStorage)[0 .. fibers.length];
        }
        foreach(int idx, ref f;this.fibers)
        {
            version (none)
            {
                f = (cast(TaskFiber)(this.fiberPoolStorage + (aligned_size * idx)));
                f.__ctor(null, &this, idx);
            }
            f = new TaskFiber(null, &this, idx);
        }
    }

    bool isInitialized()
    {
        pragma(inline, true);
        return fibers[0] !is null;
    }

    void free(TaskFiber* fiber)
    {
        const fiberIdx = fiber.idx;
        assert(fiberIdx < fibers.length);
        freeBitfield |= (1 << fiberIdx);
    }

    TaskFiber* getNext() return
    {
        if (n_free())
        {
            const fiberIdx = nextFree();
            assert(fiberIdx != INVALID_FIBER_IDX);
            freeBitfield &= ~(1 << fiberIdx);
            return &fibers[fiberIdx];
        }
        assert(0);
        //return null;
    }
}

// debug = ImmediateTaskCompletion;
/// Ticket Lock ordered syncronisation mechanism
struct TicketCounter
{
    shared align(16) uint nextTicket;
    shared align(16) uint currentlyServing;

    uint drawTicket() shared
    {
        return atomicFetchAdd(nextTicket, 1);
    }

    void releaseTicket(uint ticket) shared
    {
        assert(ticket == currentlyServing);
        atomicFetchAdd(currentlyServing, 1);
    }

    bool servingMe(uint ticket) shared
    {
        return atomicLoad(currentlyServing) == ticket;
    }

    void redrawTicket(ref uint ticket) shared
    {
        releaseTicket(ticket);
        ticket = drawTicket();
    }
}

extern (C) struct TaskQueue
{
    TicketCounter counter;

    // these can only be touched when locked
    shared Task*[32] tasks;
    align(16) shared int next_entry_to_write = 0;
    align(16) shared int next_entry_to_read = -1;

    void addTaskToQueue(shared Task* task, uint queueID) shared
    {
        // printf("trying to enqueue a task\n");
        assert(queueID != 0, "A zero queueID is invalid (0 means unowned)");
        // while we have no room to insert anything there's no point in drawing a ticket
        while(next_entry_to_write >= tasks.length)
        {
            assert(next_entry_to_write == tasks.length, 
                "next entry to write should not be greater than tasks.length this indicates a logic error.");
              // possibly pause ?            
        }
        auto myTicket = counter.drawTicket();
        scope (exit)
            counter.releaseTicket(myTicket);
            

        // printf("currentlyServing: %d, nextTickit: %d\n", currentlyServing, nextTicket);

        for (;;)
        {
            if (!counter.servingMe(myTicket))
            {
                continue;
            }
            // printf(" got the lock for %d\n", myTicket);

            if (next_entry_to_write >= tasks.length)
            {
                // TODO we could also grow the queue here, since it's under our lock

                assert(next_entry_to_write == tasks.length, 
                    "next entry to write should not be greater than tasks.length this indicates a logic error.");

                // if the task queue is full we need to redraw a ticket
                counter.redrawTicket(myTicket);
                continue;
            }

            /* do locked stuff here ... */
            if (next_entry_to_write < tasks.length)
            {
                task.queueID = queueID;
                cas(&next_entry_to_read, -1, 0); 
                tasks[atomicFetchAdd(next_entry_to_write, 1)] = cast(shared)task;
                atomicFence();
                break;
            }
            else
                assert(0, "we should have made sure that the queue has room");
        }
    }

    shared(Task)* getTaskFromQueue() shared
    {
        //TODO FIXME THIS IS BROKEN
        // printf("Trying to dequeue a task\n");

        // if the next entry to read is -1 we are empty
        if (next_entry_to_read == -1) return null;

        auto myTicket = counter.drawTicket();
        scope (exit)
            counter.releaseTicket(myTicket);

        for (;;)
        {
            if (!counter.servingMe(myTicket))
            {
                // perhaps mmPause ?
                continue;
            }
            // we have the lock now
            // printf("got lock\n");
            // we may have to wait again
            if (next_entry_to_read == -1 && next_entry_to_write)
            {
                // printf("No task for us\n");
                return null;
            }
            else
            {
                auto task = tasks[next_entry_to_read];
                if (!task || task.hasCompleted_) return null;

                auto q = this;
                assert(task);
                // printf("Got a task\n");
                if (next_entry_to_write > atomicFetchAdd(next_entry_to_read, 1))
                {
                    if (next_entry_to_read == tasks.length)
                    {
                        // printf("We have gone through the whole list\n");
                        // we have gone through the whole list
                        assert(cas(&next_entry_to_read, cast(int)tasks.length, -1));
                        assert(cas(&next_entry_to_write, cast(int)tasks.length, 0));
                    }
                }
                else
                {
                    import std.algorithm : min;
                    if (!next_entry_to_write)
                        next_entry_to_read = -1;
                    else
                        next_entry_to_read = min(next_entry_to_read, (next_entry_to_write - 1));
                }
                return task;
            }
        }
    }
}

shared uint thread_counter;
void initBackgroundThreads()
{
    foreach(ref p; unorderedBackgroundFiberPools)
    {
        p = new FiberPool();
        p.initFiberPool();
    }

    foreach(ref t; unorderedBackgroundThreads)
    {
        t = new Thread(() {
            const uint thread_idx = atomicFetchAdd(thread_counter, 1);
            printf("thread_proc start thread_id: %d\n", thread_idx + 1);
            auto myQueue = &unorderedBackgroundQueue[thread_idx];
            auto myPool = &unorderedBackgroundFiberPools[thread_idx];
            
            while(true && !killThread[thread_idx])
            {
                auto task = myQueue.getTaskFromQueue();
                if (!task)
                {
                        //printf("got no work \n");
                    continue;
                }
                // printf("pulled task with queue_id %d and thread_id is: %d and myQueue is: %p\n", task.queueID, thread_idx + 1, myQueue);
                assert(task.queueID == thread_idx + 1);
                if (task.hasCompleted_)
                    continue;

                import dmd.root.rootobject;

                if (auto ro = cast(RootObject) task.taskData)
                {
                    // printf("[BackgroundThread] running for %s\n", ro.toChars());
                }
                if (task.hasCompleted_) assert(0);

                task.assignFiber();
                task.callFiber();
                if (task.hasCompleted())
                {
                    // printf("task %p has completed myQueue.next_entry_to_write: %d\n", task, myQueue.next_entry_to_write);
                    // printf("myQueue.next_entry_to_write: %d\n", myQueue.next_entry_to_write);
                }
                else
                {
                    // printf("task %p did not complete ... readding to queue\n", task);
                    myQueue.addTaskToQueue(cast(shared)task, thread_idx + 1);
                }
            }
        });
        t.start();
    }

}

void killBackgroundThreads()
{
    printf("Killing all threads\n");
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
shared (TaskFiber)* getFiberFromPool(uint queueID, shared Task* task)
{
    __gshared FiberPool g_pool;
    if (queueID == 0)
    {
        if (!g_pool.isInitialized())
        {
            g_pool.initFiberPool();
        }
    }
    shared(TaskFiber)* result = null;

    // if queueID is zero this is not a background task, and hence has no deticated queuePool
    if (queueID == 0)
    {
        if (!g_pool.isFull())
        {
            result = cast(shared(TaskFiber)*) g_pool.getNext();
            result.currentTask = task;
            result.currentTask.hasFiber = true;
            result.currentTask.currentFiber = result;
            result.hasTask = true;
        }
    }
    else
    {
        auto pools = unorderedBackgroundFiberPools;
        auto pool = unorderedBackgroundFiberPools[queueID - 1];
        // TODO initialize per Thread fiber pools in the thread handler
        assert(pool.isInitialized);
        if (!pool.isFull())
        {
            result = cast(shared(TaskFiber)*) pool.getNext();
            result.currentTask = task;
            result.currentTask.hasFiber = true;
            result.currentTask.currentFiber = result;
            result.hasTask = true;
        }
    }
    // printf("get Fiber from Pool: %p\n", result);
    return result;
}

shared struct Task
{
    shared (void*) delegate (shared void*) fn;
    shared (void*) taskData;
    TaskGroup* taskGroup;
    void* result;

    shared Task*[] children;
    shared size_t n_children_completed;

    uint queueID;
    bool isBackgroundTask;

    shared (TaskFiber)* currentFiber;
    align(16) shared bool hasCompleted_;
    align(16) shared bool hasFiber;
    align(16) shared bool fiberIsExecuting;
    align(16) shared uint originatorAppendLock;


    bool hasCompleted(string file = __FILE__, int line = __LINE__) shared
    {
        // printf("[%s:+%d]asking has Completed \n", file.ptr, line);
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
                //currentFiber = cast(shared)new TaskFiber(&this);
                currentFiber = getFiberFromPool(queueID, (cast(Task*)&this));
            }
        }
    }

    void callFiber() shared
    {
        // a TaskFiber may only be executed by the thread that created it
        // therefore it cannot happen that a task gets completed by another thread.
        assert(hasFiber && !hasCompleted_);
        {
            auto unshared_fiber = cast(TaskFiber*)currentFiber;
            if (unshared_fiber.state() != unshared_fiber.State.EXEC &&
                cas(cast()&fiberIsExecuting, false, true))
            {
                (unshared_fiber).call();
                atomicFence();
                fiberIsExecuting = false;
            }
        }
    }
}

class TaskFiber : Fiber
{
    Task* currentTask;
    FiberPool* pool;
    int idx;
    align(16) shared bool hasTask;

    this(Task* currentTask, FiberPool* pool = null, int idx = int.max)
    {

        this.pool = pool;
        this.idx = idx;

        super(&doTask, ushort.max * 8);
        // use large stack of ushort.max * 8
        // otherwise we can crash in the parser or deeper semantic

        // currentTask will be null when initalizing a FiberPool
        if (currentTask)
        {
            assert(currentTask.hasFiber && currentTask.currentFiber is null);
            this.currentTask = currentTask;
            currentTask.currentFiber = cast(shared TaskFiber*)this;
            hasTask = true;
        }
    }

    void doTask()
    {
        if (currentTask)
        {
            hasTask = true;
            // printf("Running task for '%s'\n", currentTask.taskGroup.name.ptr);
            assert(state() != State.TERM, "Attempting to start a finished task");
            currentTask.result = currentTask.fn(currentTask.taskData);
            {
                string s = stateToString(state());
                // printf("Task state after calling fn: %s\n", s.ptr);
            }
        }

    }

    static string stateToString(typeof(new Fiber((){}).state()) state)
    {
        final switch (state)
        {
            case state.TERM:
               return "Done";
            case state.HOLD:
               return "Suspended";
            case state.EXEC:
               return "Running";
        }
    }

    bool hasCompleted() shared
    {
        // printf("querying hasCompleted for '%s'\n", currentTask.taskGroup.name.ptr);
        if (!atomicLoad(hasTask))
        {
            // if a fiber has no task it must have completed at a previous point;
            // we assume that the request is stale and return true;
            return true;
        }
        assert(currentTask && currentTask.hasFiber);
        auto state = (cast()this).state();
        // printf("hasCompleted: {State: %s} {origin: %s:%d}\n", stateToString(state).ptr, currentTask.originInfo.filename.ptr, cast(int)currentTask.originInfo.line);
        if (state == State.TERM)
        {
            if (cas(&hasTask, true, false))
            {
                if (cas(&currentTask.hasCompleted_, false, true))
                {
                    // printf("n_completed before: %d\n", cast(int)atomicLoad(currentTask.taskGroup.n_completed));
                    atomicFetchAdd(currentTask.taskGroup.n_completed, 1);
                    // printf("Registering completion with taskgroup %s\n", currentTask.taskGroup.name.ptr);
                    // printf("n_completed after: %d\n", cast(int)atomicLoad(currentTask.taskGroup.n_completed));
                    assert(currentTask.hasFiber);
                    atomicStore(currentTask.hasFiber, false);
                    auto pool = cast(FiberPool*) currentTask.currentFiber.pool;
                    if (pool)
                    {
                        pool.free((cast(TaskFiber*)currentTask.currentFiber));
                    }
                    currentTask.currentFiber = null;
                    currentTask = null;
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
        import dmd.root.rootobject;
        // printf("AddTask {taskData: '%s'} {origin: %s:%d}\n", (cast(RootObject)taskData).toChars(),
        //    file.ptr, cast(int)line,
        // );

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
                //fprintf(stderr, "[TaskGroup: %s] No immediate task completion possible for '%s' (origin: %s:%d)\n", this.name.ptr, (cast(RootObject)task.taskData).toChars(),
                //    task.originInfo.filename.ptr, task.originInfo.line,
                //);
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
        //printf("runTask({n_used: %d, n_completed: %d})\n", cast(int)n_used, cast(int)n_completed);
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
/+
        if (currentTask is null)
        {
            char[512] assertMessageBuffer = '0';
            size_t messageLength = sprintf(assertMessageBuffer.ptr, 
                "All tasks seem to have completed but n_used is %zu and n_completed is %zu which means that %ld tasks have not registered completion (tg: %s)",
                n_used, n_completed, (cast(long)(n_used - n_completed)), name.ptr
            );
            assert(currentTask, cast(string)assertMessageBuffer[0 .. messageLength]);
        }
+/
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
            }
        }
    }
    
    void awaitCompletionOfAllTasks() shared
    {
        while(n_completed < n_used)
        {
            runTask();
            assert(n_used >= n_completed);
        }
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
