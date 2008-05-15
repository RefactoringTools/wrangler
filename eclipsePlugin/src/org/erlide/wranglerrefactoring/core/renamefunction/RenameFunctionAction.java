package org.erlide.wranglerrefactoring.core.renamefunction;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringAction;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class RenameFunctionAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Rename function";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new RenameFunctionRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new RenameFunctionWizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
