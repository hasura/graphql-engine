const {json, send} = require('micro');

module.exports = async (req, res) => {
    let js;
    try {
        js = await json(req);
    } catch (err) {
        send(res, 400, {'error': err.message});
    }
    let message = 'Not able to process request';

    if (js.event.op == 'INSERT' && js.table.name == 'notes') {
        message = `New note ${js.event.data.new.id} inserted, with data: ${js.event.data.new.note}`;
    } else if (js.event.op == 'UPDATE' && js.table.name == 'notes') {
        message = `note ${js.event.data.new.id} updated, with data: ${js.event.data.new.note}`;
    } else if (js.event.op == 'DELETE' && js.table.name == 'notes') {
        message = `New note ${js.event.data.old.id} deleted, with data: ${js.event.data.old.note}`;
    }

    send(res, 200, {'message': message});
};
