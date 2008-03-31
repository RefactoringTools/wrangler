package org.erlide.wranglerrefactoring.core.renamefunction;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wranglerrefactoring.core.RefactoringParameters;
import org.erlide.wranglerrefactoring.core.rename.RenameRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;

public class RenameFunctionRefactoring extends RenameRefactoring {

	public RenameFunctionRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@Override
	public String getName() {
		return "Rename function";
	}

	@Override
	protected RpcResult sendRPC(OtpErlangList searchPath, String filePath)
			throws ErlangRpcException, RpcException {
		return managedBackend.rpc("wrangler", "rename_fun", "siisx", filePath,
				parameters.getLine(), parameters.getColoumn(), newName,
				searchPath);
	}
}
