---

uid: SmartSetup.PublishingBundles

---

# Publishing SmartSetup bundles

SmartSetup bundles can be consumed locally or published to servers.

## Publishing to the community server

We provide a server for the community at https://github.com/tmssoftware/smartsetup-registry
If you want to publish your product there, you need to follow the steps:
  1. Host your project somewhere like GitHub, GitLab or Bitbucket.
  2. Create a `tmsbuild.yaml` telling SmartSetup how to compile your product, as explained in [Creating Bundles](xref:SmartSetup.CreatingBundles).
  3. Fork the [community repository](https://github.com/tmssoftware/smartsetup-registry) and create a branch. 
  4. Add a folder with your product ID, and put the `tmsbuild.yaml` file inside the folder.
  5. [Add your fork as a server](xref:SmartSetup.ConsumingBundles#adding-and-removing-servers) for testing:

  If your forked server is, for example, at https://github.com/your_name/smartsetup-registry, then issue the command:
  ```shell
  tms server-add test zipfile https://github.com/your_name/smartsetup-registry/archive/refs/heads/main.zip
  ```

  6. Disable the community server: `tms server-enable community false`
  7. Try installing your product: `tms install product.id`
  8. If all is working, create a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork) to our community server
  9. Once approved, you can remove your **test** server, and re-enable the community server.

## Publishing to a private server

Sometimes, you might just want to publish components for your organization only. For that, you need to start by creating a [zipfile server](xref:SmartSetup.ConsumingBundles##zipfile-servers). This is a zip file with folders named after your product IDs, and then inside each folder a `tmsbuild.yaml` file used to build that product.

You can host the file in the following ways:
  * You can put it in a shared folder that your organization can access. In this case, the users will have to do the command `tms server-add company_name zipfile file://path_to_the_zipfile`
  * You can host it on a webserver. In this case, the command to register them would be `tms server-add company_name zipfile https://path_to_the_zipfile`


If the server requires authentication, you can pass the extra headers needed by setting an environment variable named `SMARTSETUP_SERVER_<NameOfServer>`, where each header is separated by a semicolon (";")

For example, if you want to publish to a server named "test_one", and the server requires token-based authentication, you could do the following:

```shell
  set SMARTSETUP_SERVER_TEST_ONE="Authorization: Bearer <token>"
```

If you have more than one header, just separate them with semicolons:

```shell
  set SMARTSETUP_SERVER_TEST_ONE="header1: value1;header2: value2"
```
