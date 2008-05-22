package org.erlide.wrangler.refactoring.core.foldexpression;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.erlide.wrangler.refactoring.ui.WranglerNewDataInputPage;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class FoundExpressionSelectionInputPage extends WranglerNewDataInputPage {

	List<ExpressionCheckButton> checkButtons;

	public FoundExpressionSelectionInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Folding the selected expressions";
	}

	@Override
	protected String initLabelText() {
		return "Please select the expressions would you like to be folded:";
	}

	@Override
	protected void initListeners() {
	}

	@Override
	protected String initTitle() {
		return "To be folded expression selection";
	}

	@Override
	protected boolean performFinish() {
		setSelectedPositions();
		return super.performFinish();
	}

	@Override
	public IWizardPage getNextPage() {
		setSelectedPositions();
		return super.getNextPage();
	}

	private void setSelectedPositions() {
		ArrayList<OtpErlangTuple> selectedPositions = new ArrayList<OtpErlangTuple>();

		for (ExpressionCheckButton b : checkButtons) {
			if (b.getSelection())
				selectedPositions.add(b.getErlangPosition());
		}

		FoldExpressionRefactoring refac = (FoldExpressionRefactoring) getRefactoring();
		refac.setSelectedPositions(selectedPositions);

	}

	@Override
	protected void initExtraControls(GridLayout layout) {
		/**
		 * hopefully disables the input field
		 */
		// this.newDataText = null;
		newDataText.setVisible(false);

		FoldExpressionRefactoring refac = (FoldExpressionRefactoring) getRefactoring();
		List<OtpErlangTuple> positions = refac.getFoundPositions();
		checkButtons = new ArrayList<ExpressionCheckButton>();

		ExpressionCheckButton b;
		GridData gridData = new GridData();
		for (OtpErlangTuple t : positions) {
			b = new ExpressionCheckButton(composite, t);
			gridData.horizontalAlignment = GridData.FILL;
			gridData.horizontalSpan = 2;
			gridData.grabExcessHorizontalSpace = true;
			b.setLayoutData(gridData);
			checkButtons.add(b);
		}
	}
}
