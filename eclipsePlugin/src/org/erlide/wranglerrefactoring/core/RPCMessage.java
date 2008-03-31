/**
 * @author György Orosz
 */
// TODO:: implement the whole class, it is just a stub
package org.erlide.wranglerrefactoring.core;

import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.runtime.backend.RpcResult;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

// TODO: implemet all the cases, not just error, but exit
public class RPCMessage {

	private RpcResult result;

	/**
	 * Class constructor.
	 * 
	 * @param result
	 * @see RpcResult
	 */
	public RPCMessage(RpcResult result) {
		this.result = result;
	}

	/**
	 * Creates the (fatal) error, warning messages from the RpcResult
	 * 
	 * @return
	 */
	public RefactoringStatus getRefactongStatus() {
		RefactoringStatus status = new RefactoringStatus();
		OtpErlangObject wranglerResponse = result.getValue();
		if (wranglerResponse instanceof OtpErlangTuple) {
			OtpErlangTuple wranglerTupleResponse = (OtpErlangTuple) wranglerResponse;
			if (!wranglerTupleResponse.elementAt(0).toString().equals("ok")) {
				status.addError(wranglerTupleResponse.elementAt(1).toString()
						.replaceAll("\"", ""));
			}
		} else {
			status.addFatalError("Unknown error occured!");
		}

		return status;
	}

	public boolean isOk() {
		// TODO:: if it is not ok, from wrnalger
		return result.isOk();
	}

}
