package org.erlide.wranglerrefactoring.core.renamemodule;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.ltk.core.refactoring.Change;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wranglerrefactoring.core.RefactoringParameters;
import org.erlide.wranglerrefactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;

public class RenameModuleRefactoring extends WranglerRefactoring {

	public RenameModuleRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@Override
	public String getName() {
		return "Rename module";
	}

	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws ErlangRpcException, RpcException {
		// TODO: not fully working
		return managedBackend.rpc("wrangler", "rename_mod_eclipse", "ssx",
				filePath, newName, searchPath);
	}

	@Override
	protected Change doOtherChanges() {
		// TODO: find the new file and create the changes
		// ...
		File newFile;
		IFile f;
		return null;
	}
}
