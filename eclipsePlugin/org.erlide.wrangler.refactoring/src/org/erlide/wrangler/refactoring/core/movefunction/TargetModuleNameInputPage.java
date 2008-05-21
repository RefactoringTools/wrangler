package org.erlide.wrangler.refactoring.core.movefunction;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.erlide.wrangler.refactoring.core.renamemodule.NewModuleNameInputPage;
import org.erlide.wrangler.refactoring.util.NameChecker;

public class TargetModuleNameInputPage extends NewModuleNameInputPage {

	private Button checkIsNewModule;

	public TargetModuleNameInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Move function to another module";
	}

	@Override
	protected void initExtraControls(GridLayout layout) {
		checkIsNewModule = new Button(composite, SWT.CHECK);
		checkIsNewModule.setText("new module");
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		checkIsNewModule.setLayoutData(gridData);

		checkIsNewModule.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
				((MoveFunctionRefactoring) getRefactoring())
						.setIsNewModule(checkIsNewModule.getSelection());
			}
		});

	}

	@Override
	protected String initLabelText() {
		return "Target module name:";
	}

	@Override
	protected String initTitle() {
		return "Move function";
	}

	@Override
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {				
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!NameChecker.checkIsAtom(s)) {
					setPageComplete(false);
					setErrorMessage("Module name must be an atom!");
				} else {
					setErrorMessage(null);
					setPageComplete(true);
				}

			}

		});
	}

}
