"use strict";
const enq = require('enquirer');

function mapPromptConstructorName (name) {
    switch (name) {
        case 'SinglePrompt':
            return 'input';
        case 'Select':
            return 'select';
    }
}

exports._prompt = function (options) {
    return function () {
        // return new Promise(resolve => setTimeout(() => resolve(7), 3000));
        // console.log('Options:', options)
        // console.log(options.constructor.name)
        options.value0.type = mapPromptConstructorName(options.constructor.name)
        return enq.prompt(options.value0);
        // return enq.prompt({
        //     type: 'input',
        //     name: options.value0.name,
        //     message: options.value0.message
        // });
    }
}

exports._promptMultiple = function (options) {
    return function () {
        // console.log('Options:', options)
        // return new Promise(resolve => setTimeout(() => resolve(7), 3000));
        // console.log(options.constructor.name)
        const newOpts = options.map(
            opt => {
                opt.value0.type = mapPromptConstructorName(opt.constructor.name);
                return opt.value0;
            }
        );
        return enq.prompt(newOpts);
    }
}
