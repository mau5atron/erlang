% Chapter 13 Errors in Concurrent Programs:

% Handling errors in concurrent programs involves a completely different
% way of thinking than handling errors in sequential programs. In this
% chapter, we'll build upon the principles you learned about in ch 6, Error
% handling in sequential programs pg 87, extending the ideas to concurrent
% programs.

% We'll look at the underlying philosophy of error handling and at the
% details of how errors are propagated between processes and trapped by
% other processes. Finally, we'll round off with some small examples that
% form a basis for programming fault-tolerant software.

% Imagine a system with only one sequential process. If this process dies,
% we might be in deep trouble since no other process can help. For this
% reason, sequential languages have concentrated on the prevention
% of failure and an emphasis on defensive programming.

% In Erlang we have a large number of processes at our disposal, so the
% failure of any individual process is not so important. We usually write
% only a small amount of defensive code and instead concentrate on writing
% corrective code. We take measures to detect the errors and then correct
% them after they have occurred.

% 13.1 Error Handling Philosophy
error_handling_philosophy() ->
	% Error handling on concurrent Erlang programs is based on the idea of
	% remote detection and handling of errors. Instead of handling an error
	% in the process where the error occurs, we let the process die and
	% correct the error in some other process.

	% When we design a fault-tolerant system, we assume that errors will
	% occur, that processes will crash, and that machines will fail. Our job
	% is to detect the errors after they have occurred and correct them if
	% possible. Users of the system should not notice any failures or suffer
	% any loss of service while the error is being fixed.

	% Since we concentrate on cure rather than prevention, our systems have
	% very little defensive code; instead, we have code to clean up the
	% system after errors have occurred. This means we will concentrate on
	% how to detect errors, how to identify what has gone wrong, and how to
	% keep the system in a stable state.

	% Detecting errros and finding out why something failed is built into the
	% Erlang VM at a very low level and is part of the Erlang programming
	% language.

	% Building groups of processes that observe each other and take
	% corrective action when errors are detected is provided in the standard
	% OTP libraries and is described in Section 23.5, the supervision tree,
	% pg 396. This chapter is about the language aspects of error detection
	% and recovery.

	% The Erlang philosophy for building fault-tolerant software can be
	% summed up in two easy-to-remember phrases:

		% "Let some other process fix
		% the error" and "Let it crash."
	.

	% Let Some Other Process Fix the Error
	let_some_other_process_fix_the_error() ->
		% Processes are arranged to monitor each other for health. If a process
		% dies, some other process can observe this and perform corrective
		% actions.

		% For one process to observe another, we must create a link or monitor
		% between the processes. If the linked or monitored processes dies, the
		% observing process is informed.
		
		% Observing processes work transparently across machine boundaries, so
		% a process running on one machine can monitor the behavior of a
		% process running on a different machine. This is the basis for
		% programming fault-tolerant systems. We cannot make fault-tolerant
		% systems on one machine since the entire machine might crash, so we
		% need at least two machines. One machine performs computations, and
		% the other machines observe the first machine and take over if the
		% first machine crashes.

		% This can be thought of as an extension of handling erros in
		% sequential code.
		.


	% Let It Crash
	let_it_crash() ->
		% This will sound very strange to you if you come from a language like
		% C. In C we are taught to write defensive code. Programs should check
		% their arguments and not crash. There is a very good reason for this
		% in C: writing multiprocess code is extremely difficult and most
		% applications have only one process, so if this process crashes the
		% entire application, you're in big trouble. Unforunately, this leasds
		% to large quantities of error checking code, which is intertwined with
		% the non-error checking code.

		% In Erlang we do exactly the opposite. We build our applications in
		% two parts: a part that solves the problem is written with as little
		% defensive code as possible; we assume that all arguments to functions
		% are correct and the programs will execute without errors.

		% The part that corrects errors is often generic, so the same
		% error-correcting code can be used for many different applications.
		% For example, in database transactions if something goes wrong in the
		% middle of a transaction, we simply abort the transaction and let the
		% system restore the database to the state it was in before the error
		% occurred. In an operating system, if a process crashes, we let the
		% operating system close any open files or sockets and restore the
		% system to a stable state.

		% This leads to a clean separation of issues. We write code that solve
		% problems and code that fixes problems, but the two are not
		% intertwined. This can lead to a dramatic reduction in code volume.
		.

	% Why Crash?
	why_crash() ->
		% Crashing immediately when something goes wrong is often a very good
		% idea; in fact, it has several advantages.

			% We don't have to write defensive code to guard against errors; we
			% just crash.

			% We don't have to think about what to do; we just crash, and
			% somebody else will fix the error.

			% We don't make matters worse by performing additional computations
			% after we know that things have gone wrong.

			% We can get very good error diagnostics if we flag the first place
			% where an error occurs. Often continuing after an error has occurred
			% leads to even more errors and makes debugging even more difficult.

			% When writing error recovery code, we don't need to bother about why
			% something crashed; we just need to concentrate on cleaning up
			% afterward.

			% It simplifies the system architecture, so we can think about the
			% application and errory recovery as two separate problems, not as
			% one interleaved problem.
		.

	% Getting Some Other Guy to Fix It

		% Letting someone else fix an error rather than doing it yourself is a
		% good idea and encourages specialization. If I need surgery, I go to a
		% doctor and don't try to operate on myself.

		% If something trivial in my car goes wrong, the car's control computer
		% will try to fix it. If this fails and something big goes wrong, I
		% have to take the car to the garage, and some other guy fixes it.

		% If something trivial in an Erlang process goes wrong, I can try to
		% fix it with a catch or try statement. But if this fails and something
		% big goes wrong, I'd better just crash and let some other process fix
		% the error.

% 13.2 Error Handling Semantics:
error_handling_semantics() ->
		% Processes:
			% There are two types of processes:
				% normal processes
				% system processes

			% spawn creates a normal process. A normal process can become a system
			% process by evaluating the BIF process_flag(trap_exit, true).

		% Links:
			% Processes can be linked. If the two processes A and B are linked and
			% A terminates for any reason, an error signal will be sent to B and
			% the other way around.

		% Link sets:
			% The link set of a process P is the set of processes that are linked
			% to P.

		% Monitors:
			% Monitors are similar to links but are one-directional. If A monitors
			% B and B terminates for any reason, a "down" message will be sent to A
			% but not the other way around.

		% Messages and Error Signals:
			% Processes collaborate by exchanging messages or error signals.
			% Messages are sent using the send primitive. Error signals are sent
			% automatically when a process crashes or when a process terminates.
			% The error signals are sent to the link set of the process that
			% terminated.

		% Receipt of an error signal:
			% When a system process receives an error signal, then signal is
			% converted into a message of the form {'EXIT', Pid, Why}. Pid is the
			% identity of the process that terminated, and Why is the reason for
			% termination (sometimes called the exit reason). If the process
			% terminates without an error, then Why will be the atom normal;
			% otherwise, Why describes the error.

		% Explicit error signals:
			% A process that evaluates exit(Why) will terminate (if this code is
			% not executing within the scope of a catch or try primitive) and
			% broadcast an exit signal with the reason Why to its link set.

		% Untrappable exit signals:
			% When a system process receives a kill signal, it terminates. Kill
			% signals are generated by calling exit(Pid, kill). This signal
			% bypasses the normal error signal processing mechanism and is not
			% converted into a message. The exit signal should be reserved for
			% rogue processes that refuse to die using any of the other error
			% handling mechanisms.

	% These definitions might look complicated, but a detailed understanding
	% of how the mechanisms work is usually not necessary to write
	% fault-tolerant code. The default behavior of the system tries to do
	% "the right thing" as regard to error handling.
	.

% 13.3 Creating Links:
creating_links() ->
	% Imagine we have a set of unrelated processes; this is show on the left
	% side of the following figure. The links are represented by dashed
	% lines.

	% Refer to image_13.3.png

	% To create links, we call the primitive < link(Pid) >, which creates a
	% link between the calling process and Pid. So, if P1 calls < link(P3) >,
	% a link is created between P1 and P3.

	% After P1 calls < link(P3) >, P3 calls link(P10), and so on, we arrive
	% at the situation show on the right side of the figure. Note that the
	% link set of P1 has one element (P3), the link set of P3 has two
	% elements (P1 and P10), and so on.
	.

% 13.4 Groups of Processes That All Die Together:
groups_of_processes_that_all_die_together() ->
	% Often you'll want to create groups of processes that all die together.
	% This is a very useful invariant for arguing about the behavior of a
	% system. When processes collaborate to solve a problem and something
	% goes wrong, we can sometimes recover, but if we can't recover, we just
	% want to stop everything we were doing. This is rather like the notion
	% of a transaction: either the processes do what they were supposed to do
	% or they are all killed.

	% Assume we have some linked processes and that one of the linked
	% processes dies. For example, see P9 in the following figure. The left
	% side of the figure shows how the processes are linked before P9 dies.
	% The right side shows which processes are still alive after P9 has
	% crashed and all error signals have been processed.

	% When P9 dies, an error signal is sent to processes P4 and P10. P4 and
	% P10 also die because they are not system processes, and error signals
	% are sent to any processes they are linked to. Ultimately, the error
	% signals propagate to all the linked processes, and the entire group of
	% linked processes dies.

	% Now if any of the processes P1, P3, P4, P9, or P10 die, they all die.
	.

% 13.5 Setting up a Firewall:
setting_up_firewall() ->
	% Sometimes we don't want all of our linked processes to die, and we want
	% to stop the propagation of errors through the system. The following
	% figure illustrates this; here all linked process up to P3 die:

	% Refer to image_13.5

	% To achieve this, assume that P3 has evaluated process_flag
	% (trap_exit,true) and become a system process (meaning that it can trap
	% exit signals). This is show with a double-circle border on the right
	% side of the figure. After P9 crashed, the propagation of errors stopped
	% at P3, so P1 and P3 did not die. This is show on the right side of the
	% figure.

	% P3 functions as a firewall, stopping errors form propagating to other
	% processes in the system.
	.

% 13.6 Monitors:
monitors() ->
	% Monitors are similar to links but with several significant differences.

		% Monitors are unidirectional. If A monitors B and B dies, then A will
		% be sent an exit message but not the other way around (recall that
		% links were bidirectional, so if A and B were linked, the death of
		% either process would result in the other process being informed).

		% When a monitored process dies, a "down" message and not an exit
		% signal is sent to the monitoring process. This means that the
		% monitoring process does not have to become a system process in order
		% to handle errors.

	% Monitors are used when you want asymmetry in error handling; links are
	% used when you want symmetric error handling. Monitors are typically
	% used by servers to monitor the behvavior of clients.

	% The next section explains the semantics of the BIFs that manipulate
	% links and monitors.
	.

% 13.7 Error Handling Primitives:
error_handling_primitives() ->
	% The primitives for manipulating links and monitors and for trapping and
	% sending exit signals are as follows:

		% -spec spawn_link(Fun) -> Pid
		% -spec spawn_link(Mod, Func, Args) -> Pid
			% This behaves like spawn(Func) or spawn(Mod, Func, Args) and also
			% creates a link between the parent and child processes.

		% -spec spawn_monitor(Fun) -> {Pid, Ref}
		% -spec spawn_monitor(Mod, Func, Args) -> {Pid, Ref}
			% This is like spaw_link, but it creates a monitor rather than a
			% link. Pid is the process identifier of the newly created process,
			% and ref is a reference to the process. If the process dies with the
			% reason Why, then the message {'DOWN', Ref, process, Pid, Why} will
			% be sent to the parent process.

		% -spec process_flag(trap_exit, true)
			% This turns the current process into a system process. A system
			% process is a process that can receive and process error signals.

		% -spec link(Pid) -> true
			% This creates a link to the process Pid. Links are symmetric. If a
			% process A evaluates link(B), then it will be linked to B. The net
			% effect is the same as if B had evaluated link(A).

			% If the process Pid does not exist, then an exit noproc exception is
			% raised.

			% If A is already linked to B and evaluates link(B) (or vice versa),
			% the call is ignored.

		% -spec unlink(Pid) -> true
			% This removes any link between the current process and the process
			% Pid.

		% -spec erlang:monitor(process, Item) -> Ref
			% This sets up a monitor. Item is a Pid or a registered name of a
			% process.

		% -spec demonitor(Ref) -> true
			% This removes a monitor with a reference Ref.

		% -spec exit(Why) -> none()
			% This causes the current process to terminate with the reason Why.
			% If the clause that executes this statement is not within the scope
			% of a catch statement, then the current process will broadcast an
			% exit signal, with argument Why to all processes to which it is
			% currently linked. It will also broadcast a DOWN message to all
			% processes that are monitoring it.

		% -spec exit(Pid, Why) -> true
			% This sends an exit signal with the reason Why to the process Pid.
			% The process executing this BIF does not itself die. This can be
			% used to "fake" exit signals.

	% We can use these primitives to setup networks of processes that monitor
	% each other, which then provide a basis for building fault-tolerant
	% software.
	.

% 13.8 Programming for Fault Tolerance:
programming_for_fault_tolerance() ->
	% In this section, you'll learn a few simple techniques that can be used
	% to make fault-tolerant code. This is not the whole story of how to make
	% a fault-tolerant system, but it is the start of a story:
	.

	% Performing an Action When a Process Dies:
	performing_action_when_process_dies() ->
		% The function on_exit(Pid, Fun) watches the process Pid and evaluates
		% Fun(Why) if the process exits with the reason Why.

		% monitor(process, Pid) creates a monitor to Pid. When the process
		% dies, a DOWN message is received and calls Fun(Why).

		% To Test this, we'll define a function F that waits for a single
		% message X and then computes list_to_atom(X).

			% 1> F = fun() ->
					% receive
						% X -> list_to_atom(X)
					% end 
				% end.

			% #Fun<erl_eval.20.69967518>		
		.

	% Why Spawning and Linking Must Be an Atomic Operation:

		% Once upon a time Erlang had two primitives, spawn and link, and
		% spawn_link(Mod, Func, Args) was defined like this:

			% spawn_link(Mod, Func, Args) ->
				% Pid = spawn(Mod, Fun, Args),
				% link(Pid),
				% Pid.

		% Then an abscure bug occurred. The spawned process died before the
		% link statement was called, so the process died but no error signal
		% was generated. This bug took a long time to find. To fix this,
		% spawn_link was added as an atomic operation. Even simple-looking
		% programs can be tricky when concurrency is involved.

		% We’ll spawn this:
			% 2> Pid = spawn(F). <0.61.0>
			% And we’ll set up an on_exit handler to monitor it.
			% 3> lib_misc:on_exit(Pid,
			%                     fun(Why) ->
			%                         io:format(" ~p died with:~p~n",[Pid, Why])
			%                     end).
			% <0.63.0>
			% If we send an atom to Pid, the process will die (because it tries to evaluate list_to_atom of a nonlist), and the on_exit handler will be called.
			% 4> Pid ! hello.
			% hello
			% 5>
			% =ERROR REPORT==== 14-May-2013::10:05:42 === Error in process <0.36.0> with exit value:
			%    {badarg,[{erlang,list_to_atom,[hello],[]}]}
			
			% The function that is invoked when the process dies can, of course, perform any computation it likes: it can ignore the error, log the error, or restart the application. The choice is up to the programmer.



	% Making a Set of Processes That All Die Together
		% Suppose we want to create severl worker processes that are used to
		% solve some problem. They evaluate the functions F1, F2, and so on. If
		% any process dies, we want them all to die. We can do this by calling
		% start([F1, F2, ...]).

			% start(Fs) -> spawn(fun() ->
			% 	[spawn_link(F) || F <- Fs],
			% 	receive
			% 	after infinity -> 
			% 		true
			% 	end 
			% end).
		
		% start(Fs) spawns a process, which spawns and links the worker
		% processes and the then waits for an infinite time. If any worker
		% process dies, they all die.

		% If we want to know whether the processes have all died, we can add an
		% on_exit handler to the start process.

		% Pid = start([F1, F2, ...]), 
		% on_exit(Pid, fun(Why) ->
		% 										... the code here runs if any worker
		% 										... process dies 
		% 							end)


		% Making a process that never dies:
		make_process_never_dies() ->
			% The idea is to make a registered process that is always alive - if
			% it dies for any reason, it will be immediately restarted.

			% We can use on_exit to program this

			% use lib_misc.erl

			% keep_alive(Name, Fun) ->
			% 	register(Name, Pid = spawn(Fun)),
			% 	on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).

			% This make s a registered process called Name that evaluates spawn
			% (Fun). if the process dies for any reason, then it is restarted.

			% There is a rather subtle error in on_exit nad keep_alive. If we
			% start hard at the following two lines of code:

			% Pid = register(....)
			% on_exit(Pid, fun(X) -> ...)


			% we see that there is a possibility that the process dies in the gap
			% between these two statements. If the process dies before on_exit
			% gets evaluated, then a link will not be created, and the on_exit
			% process will not work as you expected. This could happen if two
			% programs try to evaluate keep_alive at the same time and with the
			% same value of Name. This is called a race condition -- two bits of
			% code (this bit) and the code section that performs the link
			% operation inside on_exit are racing each other. If things go wrong
			% here, your program might behave in an unexpected manner.

			% I'm not going to solve this problem here -- I'll let you think
			% about how to do this yourself. When you combine the Erlang
			% primitives spawn, spawn_monitor, register, and so on, you must
			% think carefully about possible race conditions and write your code
			% in such a way that race conditions cannot happen.

			% Now you know all there is to know about error handling. Errors that
			% cannot be trapped in sequential code flow out of the proceses where
			% they occurred, following links to other processes that can be
			% programmed to take care of the errors. All the mechanisms we have
			% described (the linking process and so on) work transparently across
			% machine boundaries.

			% Crosssing machine boundaries leads us to distributed programming.
			% Erlang processes can spawn new processes on other physical machines
			% in the network, making it easy to write distributed programs.
			% Distributed programming is the subject of the next chapter.
			.