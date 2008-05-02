package org.erlide.wranglerrefactoring.core;

import java.util.ArrayList;
import java.util.List;

import org.erlide.runtime.backend.RpcResult;
import org.erlide.wranglerrefactoring.core.exception.WranglerException;
import org.erlide.wranglerrefactoring.core.exception.WranglerRPCException;
import org.erlide.wranglerrefactoring.core.exception.WranglerRefactoringException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RPCMessage {

	private RpcResult result;

	public RPCMessage(RpcResult result) {
		this.result = result;
	}

	public List<FileChangesTuple> getResult() {
		ArrayList<FileChangesTuple> res = new ArrayList<FileChangesTuple>();

		OtpErlangTuple rpcResp = (OtpErlangTuple) result.getValue();
		OtpErlangList changedFileTupleList = (OtpErlangList) rpcResp
				.elementAt(1);

		OtpErlangTuple e;
		OtpErlangString oldPath, newPath, newContent;
		for (int i = 0; i < changedFileTupleList.arity(); ++i) {
			e = (OtpErlangTuple) changedFileTupleList.elementAt(i);
			oldPath = (OtpErlangString) e.elementAt(0);
			newPath = (OtpErlangString) e.elementAt(1);
			newContent = (OtpErlangString) e.elementAt(2);

			res.add(new FileChangesTuple(oldPath.stringValue(), newPath
					.stringValue(), newContent.stringValue()));
		}
		// TODO: non changing refactorings do need other structure
		return res;
	}

	public void checkIsOK() throws WranglerException {
		if (result.isOk()) {
			OtpErlangTuple tuple = (OtpErlangTuple) result.getValue();
			checkOkResultCases(tuple);

		} else if (!result.isOk()) {
			OtpErlangTuple tuple = (OtpErlangTuple) result.getValue();
			if (tuple.elementAt(0).equals(new OtpErlangAtom("EXIT"))) {
				OtpErlangTuple msgTuple = (OtpErlangTuple) tuple.elementAt(1);
				OtpErlangString msg = (OtpErlangString) msgTuple.elementAt(1);
				throw new WranglerRefactoringException(msg.stringValue());
			} else
				throw new WranglerRPCException();
		}
	}

	protected void checkOkResultCases(OtpErlangTuple tuple)
			throws WranglerRefactoringException {
		if (!tuple.elementAt(0).toString().equals("ok")) {
			String message = ((OtpErlangString) tuple.elementAt(1))
					.stringValue();
			throw new WranglerRefactoringException(message);
		}
	}
}
