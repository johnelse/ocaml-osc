var testData, mlPort, mlAddr, responder;

testData = File.new("test.data", "r");
mlPort = testData.getInt32;
testData.close;
mlAddr = NetAddr("127.0.0.1", mlPort);

// Use OSCresponderNode rather than the newer OSCFunc, as Travis VMs don't have
// a new enough version of SuperCollider.

responder = OSCresponderNode(
    nil,
    '/test',
    {|time, responder, packet|
        ("supercollider: packet received:" + packet).postln;
        mlAddr.sendMsg(*packet)}
    ).add;

("supercollider: will reply to port" + mlPort).postln;
("supercollider: listening for pings on port" + NetAddr.langPort).postln;
