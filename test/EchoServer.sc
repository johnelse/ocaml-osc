var responder = OSCFunc(
    {|packet, time, addr, recvPort|
        var mlPort, mlAddr;

        mlPort = packet[1];
        ("supercollider: packet received:" + packet).postln;
        ("supercollider: will reply to port" + mlPort).postln;
        mlAddr = NetAddr("127.0.0.1", mlPort);
        mlAddr.sendMsg(*packet)},
    '/test');

("supercollider: listening for pings on port" + NetAddr.langPort).postln;
