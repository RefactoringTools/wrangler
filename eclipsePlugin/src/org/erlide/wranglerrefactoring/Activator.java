package org.erlide.wranglerrefactoring;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.internal.ManagedBackend;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import erlang.ErlangCode;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.erlide.wranglerrefactoring";

	// The shared instance
	private static Activator plugin;

	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		initWrangler();
	}

	@SuppressWarnings("restriction")
	private void initWrangler() throws CoreException {
		URL url;
		try {
			Bundle b = getDefault().getBundle();
			url = FileLocator.find(b, new Path(""), null);
			url = FileLocator.resolve(url);
			String wranglerPath = new Path(url.getPath()).append("ebin")
					.toOSString();

			ManagedBackend mb = (ManagedBackend) BackendManager.getDefault()
					.getIdeBackend();
			ErlangCode.addPathA(mb, wranglerPath);

			mb.rpc("code", "load_file", "a", "wrangler");
		} catch (IOException ioe) {
			ioe.printStackTrace();
			throw new CoreException(new Status(Status.ERROR, PLUGIN_ID,
					"Could not load the ebin files!"));
		} catch (RpcException e) {
			throw new CoreException(new Status(Status.ERROR, PLUGIN_ID,
					"Could not reach the erlang node!"));
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

}
