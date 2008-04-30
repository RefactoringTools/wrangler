package org.erlide.wranglerrefactoring.core.tupletorecord;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wranglerrefactoring.core.RefactoringParameters;
import org.erlide.wranglerrefactoring.core.WranglerRefactoring;

import com.ericsson.otp.erlang.OtpErlangList;

public class TupleToRecordRefactoring extends WranglerRefactoring {

	private String newParametersName;

	public TupleToRecordRefactoring(RefactoringParameters parameters) {
		super(parameters);

	}

	public void setNewParametersName(String str) {
		newParametersName = str;
	}

	@Override
	public String getName() {
		return "Tuple to record";
	}

	@Override
	protected RpcResult sendRPC(String filePath, OtpErlangList searchPath)
			throws ErlangRpcException, RpcException {
		return managedBackend.rpc("wrangler", "tuple_to_record_eclipse",
				"siiiissx", filePath, parameters.getStartLine(), parameters
						.getStartColoumn(), parameters.getEndLine(), parameters
						.getEndColoumn(), newName, newParametersName,
				searchPath);
	}
}
