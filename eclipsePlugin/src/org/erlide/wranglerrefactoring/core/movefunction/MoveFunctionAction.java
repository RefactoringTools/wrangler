package org.erlide.wranglerrefactoring.core.movefunction;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringAction;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class MoveFunctionAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Move function";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new MoveFunctionRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new Movefunctionwizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}
}
