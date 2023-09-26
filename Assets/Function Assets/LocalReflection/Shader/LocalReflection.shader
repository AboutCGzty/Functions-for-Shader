Shader "LocalReflection"
{
    Properties
    {
        _color ("Color", Color) = (1.0, 1.0, 1.0, 1.0)
        [NoScaleOffset]_albedo ("Albedo", 2D) = "white" { }
        _tilling ("Tilling", float) = 1.0
        // [NoScaleOffset]_envCube ("Env Cube", Cube) = "white" { }
        [Toggle(LOCALREFLECTION)]_localReflection ("Local Reflection", float) = 0
        _envSize ("Env Size", vector) = (5.0, 5.0, 5.0, 0.0)
        _envCenter ("Env Center", vector) = (0.0, 0.0, 0.0, 0.0)
        [Toggle(ROTATIONDIR)]_envRotationY ("Env RotationY", float) = 0
        _rotationAngle ("Rotation Angle", Range(0.0, 360.0)) = 0.0
        [Normal][NoScaleOffset]_normal ("Normal", 2D) = "bump" { }
        _normalScale ("Normal Scale", Range(0.0, 1.0)) = 1.0
        [Normal][NoScaleOffset]_detailNormal ("Detail Normal", 2D) = "bump" { }
        _detailNormalScale ("Detail Normal Scale", Range(0.0, 1.0)) = 1.0
        [Toggle(FLIPROUGH)]_FLIPROUGH ("Flip Roughness", float) = 0
        [NoScaleOffset]_roughness ("Roughness", 2D) = "black" { }
        _roughFactor ("Roughness Factor", Range(0.0, 7.0)) = 1.0
    }

    SubShader
    {
        Tags { "RenderPipeline" = "UniversalPipeline" "RenderType" = "Opaque" "IgnoreProjector" = "True" "ShaderModel" = "4.5" }

        Pass
        {
            Name "FORWARDLIT"
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/EntityLighting.hlsl"
            #include "Assets/Function Assets/NormalBlending/Shader/Code/Function/NormalBlendingFunctions.hlsl"
            #pragma shader_feature_local LOCALREFLECTION
            #pragma shader_feature_local ROTATIONDIR
            #pragma shader_feature_local FLIPROUGH

            struct VertexInput
            {
                float4 vertex : POSITION;
                float3 normal : NORMAL;
                float4 tangent : TANGENT;
                float2 uv_mesh : TEXCOORD0;
            };

            struct VertexOutput
            {
                float4 pos_clip : SV_POSITION;
                float3 pos_world : TEXCOORD0;
                float3 normal_world : TEXCOORD1;
                float4 tangent_world : TEXCOORD2;
                float2 uv : TEXCOORD3;
            };

            CBUFFER_START(UnityPerMaterial)
                float _tilling, _rotationAngle, _normalScale, _detailNormalScale, _roughFactor;
                float4 _color, _envSize, _envCenter;
            CBUFFER_END
            TEXTURECUBE(_envCube);          SAMPLER(sampler_envCube);
            TEXTURE2D(_albedo);             SAMPLER(sampler_albedo);
            TEXTURE2D(_normal);             SAMPLER(sampler_normal);
            TEXTURE2D(_detailNormal);       SAMPLER(sampler_detailNormal);
            TEXTURE2D(_roughness);          SAMPLER(sampler_roughness);

            VertexOutput vert(VertexInput v)
            {
                VertexOutput o;
                o.uv = v.uv_mesh * _tilling;
                VertexNormalInputs normalInputs = GetVertexNormalInputs(v.normal);
                o.normal_world = normalInputs.normalWS;
                real sign = v.tangent.w * GetOddNegativeScale();
                float4 tangentWS = float4(normalInputs.tangentWS.xyz, sign);
                o.tangent_world = tangentWS;
                VertexPositionInputs vertexInputs = GetVertexPositionInputs(v.vertex.xyz);
                o.pos_world = vertexInputs.positionWS;
                o.pos_clip = vertexInputs.positionCS;
                return o;
            }

            float3 RotationY(float angle, float3 reflecDir)
            {
                // 角度转弧度公式
                float rotationAngle = angle * PI / 180.0;
                // 构建二维旋转矩阵
                float2x2 m_Rotation = float2x2(cos(rotationAngle), -sin(rotationAngle), sin(rotationAngle), cos(rotationAngle));
                // 修改反射向量
                float2 rotationDir = mul(m_Rotation, reflecDir.xz);
                reflecDir = float3(rotationDir.x, reflecDir.y, rotationDir.y);

                return reflecDir;
            }

            float3 LocalReflection(float3 reflecDir, float3 worldPosition, float3 boxSize, float3 boxCenter)
            {
                float3 a = (-boxSize - worldPosition) / reflecDir;
                float3 b = (boxSize - worldPosition) / reflecDir;
                float3 c = max(a, b);
                float d = min(min(c.x, c.y), c.z);

                float3 finalReflectionDir = d * reflecDir + worldPosition + boxCenter;
                return normalize(finalReflectionDir);
            }

            float4 frag(VertexOutput i) : SV_Target
            {
                float3 normalMap = UnpackNormalScale(SAMPLE_TEXTURE2D(_normal, sampler_normal, i.uv), _normalScale);
                float3 detailNormalMap = UnpackNormalScale(SAMPLE_TEXTURE2D(_detailNormal, sampler_detailNormal, i.uv), _detailNormalScale);
                float3 worldNormal = normalize(i.normal_world);
                float3 worldTangent = normalize(i.tangent_world.xyz);
                float3 worldBiTangent = normalize(cross(worldNormal, worldTangent) * i.tangent_world.w);
                float3x3 matrix_TBN = float3x3(worldTangent, worldBiTangent, worldNormal);
                float3 finalNormalBlend = NormalBlending_ReorientedNormalMapping(normalMap, detailNormalMap);
                finalNormalBlend = normalize(mul(finalNormalBlend, matrix_TBN));

                float3 cameraPosition = normalize(_WorldSpaceCameraPos.xyz - i.pos_world.xyz);
                float3 reflecDir = normalize(reflect(-cameraPosition, finalNormalBlend));
                #ifdef LOCALREFLECTION
                    reflecDir = LocalReflection(reflecDir, i.pos_world.xyz, _envSize.xyz, _envCenter.xyz);
                #endif
                #ifdef ROTATIONDIR
                    reflecDir = RotationY(_rotationAngle, reflecDir);
                #endif
                // 采样 Refelection Probe
                float roughnessMap = SAMPLE_TEXTURE2D(_roughness, sampler_roughness, i.uv).r;
                #ifdef FLIPROUGH
                    roughnessMap = 1.0 - roughnessMap;
                #endif
                float roughMip = roughnessMap * (1.70 - 0.70 * roughnessMap) * _roughFactor;
                float4 envCubemap = SAMPLE_TEXTURECUBE_LOD(unity_SpecCube0, samplerunity_SpecCube0, reflecDir, roughMip);
                float3 envCubemapHDR = DecodeHDREnvironment(envCubemap, unity_SpecCube0_HDR);
                // 采样自定义 Cubemap
                // float4 envCubemap = texCUBElod(_envCube, float4(reflecDir, _roughnessLevel * 6.0));
                // float3 envCubemapHDR = DecodeHDR(envCubemap, _envCube_HDR);
                float3 bascol = SAMPLE_TEXTURE2D(_albedo, sampler_albedo, i.uv).xyz * _color.xyz;
                bascol += envCubemapHDR * bascol + envCubemapHDR * 0.3;

                return float4(bascol, 1.0);
            }
            ENDHLSL
        }
    }
}