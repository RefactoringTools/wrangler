package org.erlide.wranglerrefactoring.core.tupleparameters;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringAction;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class TupleParametersAction extends WranglerRefactoringAction {

	@Override
	public void dispose() {
		// TODO Auto-generated method stub
	}

	@Override
	protected String initRefactoringName() {
		return "Tuple function parameters";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new TupleParametersRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new TupleParametersWizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
