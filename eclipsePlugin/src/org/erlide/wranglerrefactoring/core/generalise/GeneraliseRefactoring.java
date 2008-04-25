package org.erlide.wranglerrefactoring.core.generalise;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wranglerrefactoring.core.RefactoringParameters;
import org.erlide.wranglerrefactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class GeneraliseRefactoring extends WranglerRefactoring {

	public GeneraliseRefactoring(RefactoringParameters parameters) {
		super(parameters);
	}

	@Override
	public String getName() {
		return "Generalise function";
	}

	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws ErlangRpcException, RpcException {
		// TODO: generalise into parameters!!!
		OtpErlangTuple startPos = createPos(parameters.getStartLine(),
				parameters.getStartColoumn());
		OtpErlangTuple endPos = createPos(parameters.getEndLine(), parameters
				.getEndColoumn());
		return managedBackend.rpc("wrangler", "generalise_eclipse", "sxxsx",
				filePath, startPos, endPos, newName, searchPath);
	}
}
