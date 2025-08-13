using System;
using System.Collections.Generic;

public static class TelemetryBuffer
{
    private const byte bufferTypeUInt16 = sizeof(ushort);
    private const byte bufferTypeUInt32 = sizeof(uint);
    private const byte bufferTypeInt16 = 256 - sizeof(short);
    private const byte bufferTypeInt32 = 256 - sizeof(int);
    private const byte bufferTypeInt64 = 256 - sizeof(long);

    private static byte determineBufferType(long value) => value switch
    {
        > uint.MaxValue => bufferTypeInt64,     //  4_294_967_296   9_223_372_036_854_775_807   long
        > int.MaxValue => bufferTypeUInt32,     //  2_147_483_648   4_294_967_295   uint
        > ushort.MaxValue => bufferTypeInt32,   //  65_536  2_147_483_647   int
        >= ushort.MinValue => bufferTypeUInt16, //  0   65_535  ushort
        >= short.MinValue => bufferTypeInt16,   // -32_768  -1  short
        >= int.MinValue => bufferTypeInt32,     // -2_147_483_648   -32_769 int
        >= long.MinValue => bufferTypeInt64,    // -9_223_372_036_854_775_808   -2_147_483_649  long
    };

    public static byte[] ToBuffer(long reading)
    {
        var bufferType = determineBufferType(reading);
        var buffer = new byte[9] { bufferType, 0, 0, 0, 0, 0, 0, 0, 0 };

        (bufferType switch
        {
            bufferTypeInt16 => BitConverter.GetBytes((short)reading),
            bufferTypeInt32 => BitConverter.GetBytes((int)reading),
            bufferTypeInt64 => BitConverter.GetBytes((long)reading),
            bufferTypeUInt16 => BitConverter.GetBytes((ushort)reading),
            bufferTypeUInt32 => BitConverter.GetBytes((uint)reading),
            _ => [],

        }).CopyTo(buffer, 1);

        return buffer;
    }

    public static long FromBuffer(byte[] buffer)
    {
        return buffer[0] switch
        {
            bufferTypeInt16 => BitConverter.ToInt16(buffer, 1),
            bufferTypeInt32 => BitConverter.ToInt32(buffer, 1),
            bufferTypeInt64 => BitConverter.ToInt64(buffer, 1),
            bufferTypeUInt16 => BitConverter.ToUInt16(buffer, 1),
            bufferTypeUInt32 => BitConverter.ToUInt32(buffer, 1),
            _ => 0,
        };
    }
}
