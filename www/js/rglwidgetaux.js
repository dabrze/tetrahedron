// rglwidgetaux control for querying shiny rglwiget

var rglwidgetauxBinding = new Shiny.InputBinding();
$.extend(rglwidgetauxBinding, {
  find: function(scope) {
    return $(scope).find(".rglWidgetAux");
  },
  getValue: function(el) {
    return el.value;
  },
  setValue: function(el, value) {
     el.value = value;
  },
  getState: function(el) {
    return { value: this.getValue(el) };
  },
   receiveMessage: function(el, data) {
    var $el = $(el);

    switch (data.cmd) {
       case "test":alert("Recieved Message");
                    break;
       case "getpar3d":
                    var rglel = $("#"+data.rglwidgetId);
                    if (rglel.length===0){
                       alert("bad rglwidgetId:"+ data.rglwidgetId);
                       return null;
                    }
                    var rglinst = rglel[0].rglinstance;
                    var sid = rglinst.scene.rootSubscene;
                    var par3d = rglinst.getObj(sid).par3d;
                    this.setValue(el,JSON.stringify(par3d));
                    $el.trigger("change");
                    break;
    }
  },  
  subscribe: function(el, callback) {
    $(el).on("change.rglwidgetauxBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".rglwidgetauxBinding");
  }
});
Shiny.inputBindings.register(rglwidgetauxBinding);
