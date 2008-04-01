package org.erlide.wranglerrefactoring.generalise;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringAction;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class GeneraliseAction extends WranglerRefactoringAction {

	@Override
	public void dispose() {
		// TODO Auto-generated method stub
	}

	@Override
	protected String initRefactoringName() {
		return "Generalise function";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new GeneraliseRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new GeneraliseWizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}
}
