// Use OSCresponderNode rather than the newer OSCFunc, as Travis VMs don't have
// a new enough version of SuperCollider.

var responder = OSCresponderNode(
    nil,
    '/test',
    {|time, responder, packet|
        var mlPort, mlAddr;

        mlPort = packet[1];
        ("supercollider: packet received:" + packet).postln;
        ("supercollider: will reply to port" + mlPort).postln;
        mlAddr = NetAddr("127.0.0.1", mlPort);
        mlAddr.sendMsg(*packet)}
    ).add;

("supercollider: listening for pings on port" + NetAddr.langPort).postln;
