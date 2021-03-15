module dmd.taskgroup;

import core.thread.fiber;
import core.stdc.stdio;

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

struct Task
{
    void* function (void*) fn;
    void* taskData;
    Task*[] children;
    size_t n_children_completed;

    TaskFiber currentFiber;
    bool hasCompleted;

    debug (task)
    {
        OriginInformation originInfo;
    }
}

class TaskFiber : Fiber
{
    Task* currentTask;

    this(Task* currentTask)
    {
        super(&doTask);
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
    
    Task* addTask(void* function (void*) fn, void* taskData, Task* originator = getCurrentTask(), size_t line = __LINE__, string file = __FILE__)
    {
        if (tasks.length <= n_used)
        {
            size_t n_tasks;

            if (!tasks.length)
                n_tasks = 1;
            else
                n_tasks = cast(size_t) (tasks.length * 1.2);
            
            allocate_tasks(n_tasks);
        }

        auto task = &tasks[n_used++]; 
        *task = Task(fn, taskData);
        if (originator)
            originator.children ~= task;

        debug (task)
        {
            *task.originInfo = OriginInformation(file, line, originator);
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
