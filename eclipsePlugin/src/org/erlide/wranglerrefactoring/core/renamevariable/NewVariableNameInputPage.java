package org.erlide.wranglerrefactoring.core.renamevariable;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.erlide.wranglerrefactoring.ui.WranglerNewDataPage;

public class NewVariableNameInputPage extends WranglerNewDataPage {

	public NewVariableNameInputPage(String name) {
		super(name);
	}

	@Override
	/**
	 * Erlang variable name has to start with a Capitalized character
	 */
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				// TODO: not exact, whitespaces??
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!s.substring(0, 1).toUpperCase().equals(
						s.substring(0, 1))) {
					setPageComplete(false);
					setErrorMessage("Variable name must start with an uppercase letter!");
				} else {
					setPageComplete(true);
					setErrorMessage(null);
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

	@Override
	protected void initExtraControls(GridLayout layout) {
		// TODO Auto-generated method stub

	}
}
