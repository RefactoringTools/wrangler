package org.erlide.wranglerrefactoring.core.generalise;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.erlide.wranglerrefactoring.ui.WranglerNewDataInputPage;

public class NewParameterNameInputPage extends WranglerNewDataInputPage {

	public NewParameterNameInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Generalise the selected function";
	}

	@Override
	protected String initLabelText() {
		return "New parameter name:";
	}

	@Override
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
				}

			}

		});
	}

	@Override
	protected String initTitle() {
		return "Genralise function";
	}

	@Override
	protected void initExtraControls(GridLayout layout) {
		// TODO Auto-generated method stub

	}

}
