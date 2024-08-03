
core.RefreshBox = async (args, env) => {
    const event = await interpretate(args[1], env);
    const interval = await interpretate(args[2], env);

    await interpretate(args[0], {...env});

    console.log({event, interval});

    if (interval > 0) {

        env.local.timer = setInterval(() => {
            console.log('Fire!', event);
            server.kernel.emitt(event, 'True');
        }, interval);
    }
}

core['Animate`Shutter'] = async (args, env) => {
    const dataset = await interpretate(args[1], env);
    const rate = await interpretate(args[2], env);

    let index = 0;
    setInterval(() => {
        core[args[0]].data = ['JSObject', dataset[index]];
        for (const inst of Object.values(core[args[0]].instances)) {
            inst.update();
        };

        index++;
        if (index >= dataset.length) index = 0;
    }, (1/rate) * 1000);
}

core.RefreshBox.destroy = async (args, env) => {
    if (env.local.timer) clearInterval(env.local.timer);
    console.log('Time has been stopped');
}

core.RefreshBox.virtual = true;