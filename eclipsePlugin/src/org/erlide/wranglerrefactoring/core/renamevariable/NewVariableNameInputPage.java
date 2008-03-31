package org.erlide.wranglerrefactoring.core.renamevariable;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.erlide.wranglerrefactoring.ui.WranglerNewNameInputPage;

public class NewVariableNameInputPage extends WranglerNewNameInputPage {

	public NewVariableNameInputPage(String name) {
		super(name);
	}

	@Override
	/**
	 * Erlang variable name has to start with a Capitalized character
	 */
	protected void initListeners() {
		newNameText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				// TODO: is it working?
				String s = newNameText.getText();
				RenameVariableRefactoring refac = (RenameVariableRefactoring) getRefactoring();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!s.substring(0, 1).toUpperCase().equals(
						s.substring(0, 1))) {
					setPageComplete(false);
					setErrorMessage("Variable name must start with uppercase letter!");
				} else {
					setPageComplete(true);
				}

			}

		});
	}

	@Override
	protected String initDescription() {
		return "Rename the selected variable";
	}

	@Override
	protected String initTitle() {
		return "Rename variable";
	}

	@Override
	protected String initLabelText() {
		return "New variable name:";
	}
}
