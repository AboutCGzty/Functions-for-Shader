#ifndef NORMALBLENDINGFUNCTIONS
#define NORMALBLENDINGFUNCTIONS

float3 NormalBlending_Linear(float3 n1, float3 n2)
{
    n1 = n1 * 0.5 + 0.5;
    n2 = n2 * 0.5 + 0.5;
    float3 r = (n1 + n2) * 2.0 - 2.0;
    return normalize(r);
}

float overlay(float x, float y)
{
    if (x < 0.5)
        return 2.0 * x * y;
    else
        return 1.0 - 2.0 * (1.0 - x) * (1.0 - y);
}

float3 NormalBlending_Overlay(float3 n1, float3 n2)
{
    n1 = n1 * 0.5 + 0.5;
    n2 = n2 * 0.5 + 0.5;
    float3 n;
    n.x = overlay(n1.x, n2.x);
    n.y = overlay(n1.y, n2.y);
    n.z = overlay(n1.z, n2.z);
    return normalize(n * 2 - 1);
}

float3 NormalBlending_PartialDerivative(float3 n1, float3 n2, float blend)
{
    float2 pd = lerp(n1.xy / n1.z, n2.xy / n2.z, blend);
    return normalize(float3(pd, 1));
}

float3 NormalBlending_Whiteout(float3 n1, float3 n2)
{
    return normalize(float3(n1.xy + n2.xy, n1.z * n2.z));
}

float3 NormalBlending_ReorientedNormalMapping(float3 n1, float3 n2)
{
    n1.z += 1.0;
    n2.xy = -n2.xy;
    return normalize(n1 * dot(n1, n2) - n2 * n1.z);
}

#endif