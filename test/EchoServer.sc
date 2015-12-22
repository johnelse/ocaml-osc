var testData, mlPort, mlAddr, responder;

testData = File.new("test.data", "r");
mlPort = testData.getInt32;
testData.close;
mlAddr = NetAddr("127.0.0.1", mlPort);

responder = OSCFunc(
    {|packet, time, addr, recvPort|
        ("supercollider: packet received:" + packet).postln;
        ("supercollider: will reply to port" + mlPort).postln;
        mlAddr.sendMsg(*packet)},
    '/test');

("supercollider: will reply to port" + mlPort).postln;
("supercollider: listening for pings on port" + NetAddr.langPort).postln;
