/**
 * @author György Orosz
 */
package org.erlide.wranglerrefactoring.core;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.internal.ManagedBackend;
import org.erlide.wranglerrefactoring.Activator;
import org.erlide.wranglerrefactoring.core.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

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

	protected String newName;

	private RPCMessage message;

	public WranglerRefactoring(RefactoringParameters parameters) {
		this.parameters = parameters;
		this.managedBackend = (ManagedBackend) BackendManager.getDefault()
				.getIdeBackend();
	}

	public void setNewName(String newName) {
		this.newName = newName;
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
		try {
			doRefactoring();
		} catch (WranglerException e) {
			rs.addError(e.getLocalizedMessage());
		} catch (IOException e) {
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

		/*
		 * RPCMessage message = new RPCMessage(rpcResult); rs =
		 * message.getRefactongStatus();
		 */
		return rs;
	}

	/*
	 * 
	 * OLD VERSION // TODO: dispose(), but when? protected RpcResult
	 * doRefactoring() throws IOException, CoreException, ErlangRpcException,
	 * RpcException { // TODO:: it is just a stub, some information is needed,
	 * and then create // an abstract method to the rpc call // 1. create copier
	 * object, copy the files // 2. call the abstract method see upper // 3.
	 * create changes, with the object, described in the 1, // maybe itt will be
	 * 2 class: A) just the copier, B) interface upon the // copier and upon the
	 * fileDiff ProjectCopier pc = new ProjectCopier(parameters.getFile());
	 * pc.doCopy();
	 * 
	 * OtpErlangList searchPathList = new OtpErlangList(new OtpErlangString(pc
	 * .getSearchPath()));
	 * 
	 * RpcResult res = sendRPC(pc.getFilePath(), searchPathList);
	 * 
	 * doOtherChanges();
	 * 
	 * change = pc.createChanges();
	 * 
	 * pc.dispose();
	 * 
	 * return res; }
	 */

	protected void doRefactoring() throws ErlangRpcException, RpcException,
			WranglerException, IOException, CoreException {

		String filePath = parameters.getFilePath();
		RpcResult res = sendRPC(filePath, parameters.getProject());

		message = new RPCMessage(res);
		message.checkIsOK();
	}

	protected Change doOtherChanges() {
		return null;
	}

	protected abstract RpcResult sendRPC(String filePath,
			OtpErlangList searchPath) throws ErlangRpcException, RpcException;

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
		CompositeChange cChange = new CompositeChange("wrangler made changes");

		List<FileChangesTuple> fileRs = message.getResult();
		try {
			Change c;
			for (FileChangesTuple e : fileRs) {
				c = e.createChanges();
				if (!c.equals(null))
					cChange.add(c);
			}
		} catch (IOException e) {
			Status s = new Status(Status.ERROR, Activator.PLUGIN_ID, e
					.getMessage());
			throw new CoreException(s);
		}

		Change otherChange = doOtherChanges();
		if (null != otherChange) {
			int a;
			cChange.add(otherChange);
		}

		change = cChange;

		return change;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#getName()
	 */
	@Override
	public abstract String getName();

	static protected OtpErlangTuple createPos(Integer line, Integer coloumn) {
		return new OtpErlangTuple(new OtpErlangInt(line), new OtpErlangInt(
				coloumn));
	}
}
