package org.erlide.wrangler.refactoring.core.renamemodule;

import java.util.Map;

import org.eclipse.ltk.core.refactoring.RefactoringContribution;
import org.eclipse.ltk.core.refactoring.RefactoringDescriptor;
import org.eclipse.ltk.core.refactoring.participants.RenameParticipant;

public class RenameModuleRefactoringContribution extends
		RefactoringContribution {

	public RenameModuleRefactoringContribution() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public RefactoringDescriptor createDescriptor(String id, String project,
			String description, String comment, Map arguments, int flags) {
		RenameParticipant r;
		return null;
	}
}
