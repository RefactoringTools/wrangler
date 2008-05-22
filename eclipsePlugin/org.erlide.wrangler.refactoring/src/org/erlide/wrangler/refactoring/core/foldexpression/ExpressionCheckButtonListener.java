package org.erlide.wrangler.refactoring.core.foldexpression;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Widget;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;

import com.ericsson.otp.erlang.OtpErlangRangeException;

public class ExpressionCheckButtonListener implements SelectionListener,
		MouseTrackListener {

	private ITextSelection selection;
	private RefactoringParameters parameters;

	public ExpressionCheckButtonListener(RefactoringParameters parameters) {
		this.selection = parameters.getSelection();
		this.parameters = parameters;
	}

	// probably the SelectionListener is not needed, because the button's
	// internal state is automatically changed
	@Override
	public void widgetDefaultSelected(SelectionEvent e) {
	}

	@Override
	public void widgetSelected(SelectionEvent e) {
	}

	@Override
	public void mouseEnter(MouseEvent e) {
		setHighlight(e.widget);
	}

	@Override
	public void mouseExit(MouseEvent e) {
		resetHighlight();
	}

	@Override
	public void mouseHover(MouseEvent e) {
	}

	private void setHighlight(Widget w) {
		if (w instanceof ExpressionCheckButton) {

			ExpressionCheckButton button = (ExpressionCheckButton) w;

			try {
				int startOffset = parameters
						.calculateOffsetFromErlangPos(button.getStartingPos());
				int endOffset = parameters.calculateOffsetFromErlangPos(button
						.getStartingPos());
				parameters.getEditor().setHighlightRange(startOffset,
						endOffset - startOffset, true);
			} catch (OtpErlangRangeException e) {
				e.printStackTrace();
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
		}
	}

	private void resetHighlight() {
		parameters.getEditor().setHighlightRange(selection.getOffset(),
				selection.getLength(), true);
	}
}
