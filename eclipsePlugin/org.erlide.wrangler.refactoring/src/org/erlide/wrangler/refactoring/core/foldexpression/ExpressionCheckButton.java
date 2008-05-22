package org.erlide.wrangler.refactoring.core.foldexpression;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class ExpressionCheckButton extends Button {

	private OtpErlangTuple position;

	// private boolean selected;

	public ExpressionCheckButton(Composite parent, OtpErlangTuple position) {
		super(parent, SWT.CHECK);
		this.position = position;
		this.setText(getPosExpression());
	}

	private String getPosExpression() {
		return position.elementAt(2).toString();
	}

	public OtpErlangTuple getErlangPosition() {
		return position;
	}

	public OtpErlangTuple getStartingPos() {
		OtpErlangTuple startPos = (OtpErlangTuple) position.elementAt(0);
		return startPos;
	}

	public OtpErlangTuple getEndingPos() {
		OtpErlangTuple startPos = (OtpErlangTuple) position.elementAt(1);
		return startPos;
	}

	/*
	 * public boolean isSelected() { return selected; }
	 * 
	 * public void setSelected(boolean s) { selected = s; }
	 */

}
