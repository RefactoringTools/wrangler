package org.erlide.wranglerrefactoring.core.renamevariable;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringAction;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class RenameVariableAction extends WranglerRefactoringAction {

	@Override
	public void dispose() {
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new RenameVariableRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new RenameVariableWizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);

	}

	@Override
	protected String initRefactoringName() {
		return "Rename variable";
	}

}
