import dmd.taskgroup;
import std.conv : to;
import core.time;

void main(string[] args)
{
    import core.memory : GC;
    GC.disable();
    bool multithread = false;
    if (args.length > 1 && args[1] == "mt")
        multithread = true;

    import std.stdio; 
    writeln("multithreaded: ", multithread);

    bool task1_completed = 0;
    bool task2_completed = 0;

    enum child_tasks = 128;

    if (multithread)
        initBackgroundThreads();

    scope(exit)
        if (multithread)
            killBackgroundThreads();

    size_t line = __LINE__;
    shared TaskGroup tg = TaskGroup("tg1", 3 + (child_tasks * 2), TaskGroupFlags.None);
    tg.addTask((shared void* x) { // line + 2
        foreach(i; 0 .. child_tasks)
        {
        auto task1 = tg.addTask((shared void* x) { // line + 5
            task1_completed = true;
            import core.thread;
            Thread.sleep(1.msecs); // simulate work
            return null;
        }, x, multithread);
        auto task2 = tg.addTask((shared void* x) { // line + 11
            __gshared int c;
//            writeln("completed: ", c++);
            import core.thread;
            Thread.sleep(1.msecs); // simulate work
            task2_completed = true;
            return null;
        }, x, multithread);
        assert(task1.originInfo.line == line + 5);
        assert(task2.originInfo.line == line + 11);
        assert(task1.originInfo.originator.originInfo.line == line + 2 );

        }   
        return x;
    }, null, multithread);

    tg.awaitCompletionOfAllTasks();

    import std.stdio;
    // writeln(taskGraph(&tg.tasks[0]));

    assert(task1_completed);
    assert(task2_completed);
}
