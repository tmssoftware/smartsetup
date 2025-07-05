---

uid: SmartSetup.ConsumingBundles

---

# Consuming SmartSetup bundles

## Understanding servers

SmartSetup 2.0 introduced the concept of **Servers**. A Server is just a place that can provide multiple SmartSetup products for you to install.

Currently, SmartSetup comes with three predefined servers, but you a

  * Local Servers
  * Api Servers
  * ZipFile Servers

  Only Api servers support authentication. For that reason, and to keep the interface simpler, at the moment we only allow a single active Api server. 