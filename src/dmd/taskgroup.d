

module dmd.taskgroup;

import core.thread.myFiber;
import core.stdc.stdio;
import core.thread.osthread;
import core.atomic;
import dmd.root.ticket;

@("tracy"):
enum n_threads = 4;

__gshared Thread[n_threads] unorderedBackgroundThreads;
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
@("tracy"):
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
import dmd.root.ticket;

extern (C) struct TaskQueue
{
@("tracy"):
    shared TicketCounter queueLock;

    // these can only be touched when locked
    shared Task*[4096] tasks;
    align(16) shared int next_entry_to_write = 0;
    align(16) shared int next_entry_to_read = -1;

    void addTaskToQueue(shared Task* task, uint queueID) shared
    {
        // auto q = this;  
        // printf("trying to enqueue a task\n");
        assert(queueID != 0, "A zero queueID is invalid (0 means unowned)");
        // while we have no room to insert anything there's no point in drawing a ticket
        while(next_entry_to_write > tasks.length)
        {
            assert(next_entry_to_write == tasks.length, 
                "next entry to write should not be greater than tasks.length this indicates a logic error.");
            // possibly pause ?            
        }
        auto myTicket = queueLock.drawTicket();
        scope (exit)
        {
            queueLock.releaseTicket(myTicket);
            assert(task);
        }
            

        // printf("currentlyServing: %d, nextTickit: %d\n", queueLock.currentlyServing, queueLock.nextTicket);

        for (;;)
        {
            if (!queueLock.servingMe(myTicket))
            {
                continue;
            }
            //printf("got the lock for %d\n", myTicket);

            if (next_entry_to_write >= tasks.length)
            {
                // TODO we could also grow the queue here, since it's under our lock

                assert(next_entry_to_write == tasks.length, 
                    "next entry to write should not be greater than tasks.length this indicates a logic error.");

                // if the task queue is full we need to redraw a ticket
                // printf("No more entries ... redrawing\n");

                queueLock.redrawTicket(myTicket);
                continue;
            }

            /* do locked stuff here ... */
            if (atomicLoad(next_entry_to_write) < tasks.length)
            {
                task.queueID = queueID;
                cas(&next_entry_to_read, -1, 0);
                if (next_entry_to_write < tasks.length)
                {
                    // printf("added task to queue in pos %d\n", next_entry_to_write);
                    tasks[next_entry_to_write] = cast(shared)task;
                    atomicOp!"+="(this.next_entry_to_write, 1);
                    atomicFence();
                    break;
                }
                else
                {
                    // printf("No more entries ... redrawing\n");
                    queueLock.redrawTicket(myTicket);
                    continue;
                }
            }
            else
                assert(0, "we should have made sure that the queue has room");
                



        }
    }

    shared(Task)* getTaskFromQueue() shared
    {
        auto q = this;
        //TODO FIXME THIS IS BROKEN
        //printf("Trying to dequeue a task\n");

        // if the next entry to read is -1 we are empty
        if (next_entry_to_read == -1)
        {
             // printf("{early exit} TaskQueue is empty.\n");

            return null;
        }

        auto myTicket = queueLock.drawTicket();
        scope (exit)
            queueLock.releaseTicket(myTicket);

        for (;;)
        {
            if (!queueLock.servingMe(myTicket))
            {
                // perhaps mmPause ?
                continue;
            }
            // we have the lock now
            // printf(__FUNCTION__ ~ " got lock for %d\n", myTicket);
            // we may have to wait again
            if (next_entry_to_read == -1 && next_entry_to_write)
            {
                // printf("TaskQueue is empty now.\n");
                queueLock.redrawTicket(myTicket);
                continue;
            }
            else
            {
                auto task = tasks[next_entry_to_read];
                if (!task || task.hasCompleted_) return null;

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
                // printf("Deqeued task for: %s\n", task.taskGroup.name.ptr);
                return task;
            }
        }
    }
}

shared uint thread_counter;
void initBackgroundThreads()
{
    foreach(ref t; unorderedBackgroundThreads)
    {
        t = new Thread(() {
            const uint thread_idx = atomicFetchAdd(thread_counter, 1);
            // printf("thread_proc start thread_id: %d\n", thread_idx + 1);
            auto myQueue = &unorderedBackgroundQueue[thread_idx];
            // auto myPool = &unorderedBackgroundFiberPools[thread_idx];
            auto myPool = unorderedBackgroundFiberPools[thread_idx] = new FiberPool();
            myPool.initFiberPool();
            
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
                if (!task.hasFiber)
                {
                    // technically we should aquire the taskLock here.
                    task.assignFiber();
                }
                task.callFiber();
                if (task.hasCompleted())
                {
                    // printf("task %p has completed myQueue.next_entry_to_write: %d\n", task, myQueue.next_entry_to_write);
                    // printf("myQueue.next_entry_to_write: %d\n", myQueue.next_entry_to_write);
                }
                else
                {
                    printf("task %p did not complete ... readding to queue\n", task);
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
@("tracy")
void dispatchToBackgroundThread(shared Task* task)
{
    auto ticket = task.taskLock.drawTicket();
    scope(exit) task.taskLock.releaseTicket(ticket);
    while(!task.taskLock.servingMe(ticket))
    {}

    assert(task);
    static shared uint queueCounter = 0;
    uint queueID = atomicFetchAdd(queueCounter, 1) % cast(uint)(unorderedBackgroundQueue.length);
    unorderedBackgroundQueue[queueID].addTaskToQueue(task, queueID + 1);
}
@("tracy")
shared (TaskFiber)* getFiberFromPool(uint queueID, shared Task* task)
{
    __gshared FiberPool g_pool;
    shared TicketCounter g_pool_lock;

    if (queueID == 0)
    {
        if (!g_pool.isInitialized())
        {
            auto myTicket = g_pool_lock.drawTicket();
            scope(exit)
                g_pool_lock.releaseTicket(myTicket);
            while(!g_pool_lock.servingMe(myTicket))
            {}

            if (!g_pool.isInitialized())
            {
                g_pool.initFiberPool();
            }
        }
    }
    shared(TaskFiber)* result = null;

    // if queueID is zero this is not a background task, and hence has no deticated queuePool
    if (queueID == 0)
    {
        if (!g_pool.isFull())
        {
            result = cast(shared(TaskFiber)*) g_pool.getNext();
            assert(result);
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

        assert(pool.isInitialized);
        if (!pool.isFull())
        {
            result = cast(shared(TaskFiber)*) pool.getNext();
            assert(result);
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
    invariant { if (hasFiber)
        {
            import std.stdio;
            assert(currentFiber);
            import core.stdc.stdlib : abort;
            if (!currentFiber) abort();
        }
    }
    shared (void*) delegate (shared void*) fn;
    shared (void*) taskData;
    TaskGroup* taskGroup;
    bool isBackgroundTask;

    void* result;

    shared Task*[] children;
    shared size_t n_children_completed;

    uint queueID;

    shared (TaskFiber)* currentFiber;
    align(16) shared bool hasCompleted_ = false;
    align(16) shared bool hasFiber = false;
    align(16) shared bool fiberIsExecuting = false;

    shared TicketCounter taskLock;

    Ticket creation_ticket;
    uint completion_attempts;

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
/+
        auto ticket = taskLock.drawTicket();
        scope(exit) taskLock.releaseTicket(ticket);
        while(!taskLock.servingMe(ticket))
        {}
+/
        assert(fn !is null);
        if (!atomicLoad(hasCompleted_) && !hasFiber)
        {
            if (cas(&hasFiber, false, true))
            {
                currentFiber = getFiberFromPool(queueID, (cast(Task*)&this));
                if (!currentFiber)
                    hasFiber = false;
            }
            else
            {
                // it should never happen that multiple threads try to assign a Fiber to as task!
                assert(0);
            }
        }
    }

    void callFiber() shared
    {
        auto ticket = taskLock.drawTicket();
        scope(exit) taskLock.releaseTicket(ticket);
        while(!taskLock.servingMe(ticket))
        {
            printf("TaskLoc blocked by: %s\n", taskLock.lastAquiredLoc.func.ptr);
        }

        // a TaskFiber may only be executed by the thread that created it
        // therefore it cannot happen that a task gets completed by another thread.
        assert(hasFiber && !hasCompleted_);
        {
            if (!currentFiber || fiberIsExecuting) assert(0);

            auto unshared_fiber = cast(TaskFiber*)currentFiber;
            auto state = unshared_fiber.state();
            assert(state != state.TERM, "attempting to call completed fiber ... this means we have not set ourselfs to completed the first time around\n");
            if (state == state.HOLD &&
                cas(cast()&fiberIsExecuting, false, true))
            {
                atomicOp!("+=")(completion_attempts, 1);
                // asm @trusted pure nothrow { int 3; } 
                (unshared_fiber).call();
                if (completion_attempts > 500)
                    printf("Task does not appear to complete\n");
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
            // hasTask = true;
            printf("Running task for '%s'\n", currentTask.taskGroup.name.ptr);
            assert(state() != State.TERM, "Attempting to start a finished task");
            currentTask.result = currentTask.fn(currentTask.taskData);
            {
                string s = stateToString(state());
                printf("Task state after calling fn: %s\n", s.ptr);
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
        auto ticket = currentTask.taskLock.drawTicket();
        Task* safePtrForLockRelease = currentTask;

        scope(exit) safePtrForLockRelease.taskLock.releaseTicket(ticket);
        while(!currentTask.taskLock.servingMe(ticket))
        {}

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
                    printf("Registering completion with taskgroup %s\n", currentTask.taskGroup.name.ptr);
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
@("tracy"):
    string name;
    TaskGroupFlags flags;

    shared size_t n_used;
    shared size_t n_completed;

    shared Task[] tasks;
    shared TicketCounter groupLock;

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
        auto myTicket = groupLock.drawTicket();
        scope (exit)
            groupLock.releaseTicket(myTicket);

        while(!groupLock.servingMe(myTicket))
        {
            printf("Wating for groupLock\n");
        }

        import dmd.root.rootobject;
        printf("AddTask {group: '%s'} {origin: %s:%d}\n", this.name.ptr,
           file.ptr, cast(int)line,
        );

        if (tasks.length <= n_used)
        {
            shared size_t n_tasks;

            n_tasks = tasks.length == 0 ?
                1 :
                cast(size_t) (tasks.length * 1.2);

            allocate_tasks(n_tasks);
        }

        shared task = &tasks[atomicFetchAdd(n_used, 1)];
        *task = Task(fn, taskData, &this, background_task);
        task.creation_ticket = myTicket;

/+
        if (originator) // technically we need to take a lock here!
            originator.children ~= task;
+/
        if (background_task && !(flags & TaskGroupFlags.ImmediateTaskCompletion))
        {
            dispatchToBackgroundThread(task);
        }
        if (background_task && (flags & TaskGroupFlags.ImmediateTaskCompletion))
            fprintf(stderr, "Warning: background tasks should not in an immediate completion taskgroup\n");

        //debug (task)
        {
            task.originInfo = OriginInformation(file, cast(int)line, originator);
        }

        if (flags & TaskGroupFlags.ImmediateTaskCompletion)
        {
            groupLock.releaseTicket(myTicket);
            auto myTaskTicket = task.taskLock.drawTicket();

            task.assignFiber();
            task.taskLock.releaseTicket(myTaskTicket);
            task.callFiber();
            myTicket = groupLock.drawTicket();

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
        auto myTicket = groupLock.drawTicket();
        scope (exit)
            groupLock.releaseTicket(myTicket);
        
        while(!groupLock.servingMe(myTicket))
        {}
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

        auto myTicket = groupLock.drawTicket();
        scope(exit) 
        {
            if (groupLock.servingMe(myTicket))
                groupLock.releaseTicket(myTicket);
        }
        while(!groupLock.servingMe(myTicket))
        {
            printf("Wating for groupLock .. lastAquire by: %s\n", groupLock.lastAquiredLoc.func.ptr);
        }

        foreach(ref task; tasks[0 .. n_used])
        {
            if ((!task.isBackgroundTask) && (!atomicLoad(task.hasCompleted_)))
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
            assert(!currentTask.isBackgroundTask);
            while (currentTask.children.length)
            {
                parent = currentTask;
                foreach(ref task; currentTask.children)
                {
                    if ((!task.isBackgroundTask || !MULTITHREAD) && (!atomicLoad(task.hasCompleted_)) && (!task.hasCompleted()))
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


            if ((!currentTask.isBackgroundTask || !MULTITHREAD) && currentTask.fn)
            {
                import dmd.root.rootobject;
                if (auto ro = cast(RootObject) currentTask.taskData)
                {
                    printf("running for '%s' in taskGroup: %s\n", ro.toChars(), currentTask.taskGroup.name.ptr);
                }

                assert(!currentTask.hasCompleted_);

                if (!currentTask.hasFiber)
                {
                    currentTask.assignFiber();
                }
                assert(!currentTask.isBackgroundTask);
                // after fiber assignment is done the task could try to add a new Task.
                // reqlinquish the group lock here.

                groupLock.releaseTicket(myTicket);
                currentTask.callFiber();
                currentTask.currentFiber.hasCompleted();
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
        enum formatBufferLength = 4096 * 32;
        static char[] formatBuffer = null;
        if (!formatBuffer.ptr)
        {
            formatBuffer = 
                cast(char[])malloc(formatBufferLength)[0  .. formatBufferLength];
        }

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
