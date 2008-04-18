package org.erlide.wranglerrefactoring.core.tuple;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class TupleParametersWizard extends WranglerRefactoringWizard {

	public TupleParametersWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new PatametersNumberInputPage("Get paramters number to tuple"));

	}

	@Override
	protected String initTitle() {
		return "Function parameters tuple refactoring";
	}

}
