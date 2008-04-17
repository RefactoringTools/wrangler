package org.erlide.wranglerrefactoring.core.funextraction;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringAction;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class FunExtractionAction extends WranglerRefactoringAction {

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	protected String initRefactoringName() {
		return "Fun extraction refactoring";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new FunExtractionRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new FunExtractionRefactoringWizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
