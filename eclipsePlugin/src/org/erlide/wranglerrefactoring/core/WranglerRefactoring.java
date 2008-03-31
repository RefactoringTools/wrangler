/**
 * @author György Orosz
 */
package org.erlide.wranglerrefactoring.core;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.internal.ManagedBackend;
import org.erlide.wranglerrefactoring.util.ProjectCopier;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * @author mee
 * 
 */
public abstract class WranglerRefactoring extends Refactoring {

	protected Integer coloumn, line;

	protected Change change;
	protected RefactoringParameters parameters;

	@SuppressWarnings("restriction")
	protected ManagedBackend managedBackend;

	public WranglerRefactoring(RefactoringParameters parameters) {
		this.parameters = parameters;
		this.managedBackend = (ManagedBackend) BackendManager.getDefault()
				.getIdeBackend();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#checkFinalConditions(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		RefactoringStatus rs = new RefactoringStatus();
		RpcResult rpcResult;
		try {
			rpcResult = doRefactoring();
		} catch (IOException e) {

			// TODO: is it good enough?
			rs.addError("I/O error during the refactoring:\n" + e.getMessage());
			return rs;
		} catch (ErlangRpcException e) {
			rs.addError(e.getMessage());
			return rs;
		} catch (RpcException e) {
			rs.addError(e.getMessage());
			return rs;
		} catch (Exception e) {
			// TODO: delete after alpha stage
			e.printStackTrace();
			return rs;
		}

		RPCMessage message = new RPCMessage(rpcResult);
		rs = message.getRefactongStatus();
		return rs;
	}

	// TODO: dispose(), but when?
	protected RpcResult doRefactoring() throws IOException, CoreException,
			ErlangRpcException, RpcException {
		// TODO:: it is just a stub, some information is needed, and then create
		// an abstract method to the rpc call
		// 1. create copier object, copy the files
		// 2. call the abstract method see upper
		// 3. create changes, with the object, described in the 1,
		// maybe itt will be 2 class: A) just the copier, B) interface upon the
		// copier and upon the fileDiff
		ProjectCopier pc = new ProjectCopier(parameters.getFile());
		pc.doCopy();

		OtpErlangList searchPathList = new OtpErlangList(new OtpErlangString(pc
				.getSearchPath()));

		RpcResult res = sendRPC(searchPathList, pc.getFilePath());

		change = pc.createChanges();

		pc.dispose();

		return res;
	}

	protected abstract RpcResult sendRPC(OtpErlangList searchPath,
			String filePath) throws ErlangRpcException, RpcException;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#checkInitialConditions(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {

		return new RefactoringStatus();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#createChange(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException,
			OperationCanceledException {
		return change;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#getName()
	 */
	@Override
	public abstract String getName();

}
