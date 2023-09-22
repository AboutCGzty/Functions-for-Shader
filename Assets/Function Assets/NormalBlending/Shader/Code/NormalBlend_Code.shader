Shader "NormalBlend/Code"
{
    Properties
    {
        _BaseColor ("Color", Color) = (0.8, 0.8, 0.8, 0.8)
        [Normal][NoScaleOffset]_Normal1 ("Main Normal", 2D) = "bump" { }
        _Normal1Intensity ("Main Normal Intensity", Range(0.0, 1.0)) = 1.0
        [Normal][NoScaleOffset]_Normal2 ("Detail Normal", 2D) = "bump" { }
        _Normal2Intensity ("Detail Normal Intensity", Range(0.0, 1.0)) = 1.0

        [KeywordEnum(Linear, Overlay, PD, Whiteout, RNM)]_MODE ("Blending Mode", int) = 0
    }

    SubShader
    {
        Tags
        {
            "RenderType" = "Opaque"
            "RenderPipeline" = "UniversalPipeline"
            "UniversalMaterialType" = "Lit"
            "IgnoreProjector" = "True"
            "ShaderModel" = "4.5"
        }

        Pass
        {
            Tags
            {
                "LightMode" = "UniversalForward"
            }

            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Assets/Function Assets/NormalBlending/Shader/Code/Function/NormalBlendingFunctions.hlsl"
            #pragma shader_feature_local _MODE_LINEAR _MODE_OVERLAY _MODE_PD _MODE_WHITEOUT _MODE_RNM

            struct Attributes
            {
                float4 positionOS : POSITION;
                float3 normalOS : NORMAL;
                float4 tangentOS : TANGENT;
                float2 texcoord : TEXCOORD0;
            };

            struct Varyings
            {
                float4 positionCS : SV_POSITION;
                float2 uv : TEXCOORD0;
                float3 positionWS : TEXCOORD1;
                float3 normalWS : TEXCOORD2;
                float4 tangentWS : TEXCOORD3;
            };

            CBUFFER_START(UnityPerMaterial)
                float4 _BaseColor;
                float _Normal1Intensity, _Normal2Intensity;
            CBUFFER_END
            TEXTURE2D(_Normal1);        SAMPLER(sampler_Normal1);
            TEXTURE2D(_Normal2);        SAMPLER(sampler_Normal2);
            
            Varyings vert(Attributes input)
            {
                // Initialize Vertex Outputs Struct to 0
                Varyings output = (Varyings)0;
                // Convert model vertex pisition from model space to clip space
                VertexPositionInputs vertexInput = GetVertexPositionInputs(input.positionOS.xyz);
                // Convert normal direction from model space to world space
                VertexNormalInputs normalInput = GetVertexNormalInputs(input.normalOS, input.tangentOS);
                // Assign some values to the Vertex Outputs Struct
                output.uv = input.texcoord;
                output.normalWS = normalInput.normalWS;
                // Use the model space tangent.w to judge toward
                real sign = input.tangentOS.w * GetOddNegativeScale();
                // Calculate the world space tangent
                float4 tangentWS = float4(normalInput.tangentWS.xyz, sign);
                output.tangentWS = tangentWS;
                // Calculate the world space vertex postion
                output.positionWS = vertexInput.positionWS;
                // Calculate the clip space vertex postion
                output.positionCS = vertexInput.positionCS;
                return output;
            }

            float4 frag(Varyings input) : SV_Target
            {
                float3 worldPosition = input.positionWS.xyz;
                float3 worldNormal = normalize(input.normalWS);
                float3 worldTangent = normalize(input.tangentWS.xyz);
                float3 worldBiTangent = normalize(cross(worldNormal, worldTangent) * input.tangentWS.w);
                float3x3 matrix_TBN = float3x3(worldTangent, worldBiTangent, worldNormal);

                float2 textureUV = input.uv;
                float3 n1 = UnpackNormalScale(SAMPLE_TEXTURE2D(_Normal1, sampler_Normal1, textureUV), _Normal1Intensity);
                float3 n2 = UnpackNormalScale(SAMPLE_TEXTURE2D(_Normal2, sampler_Normal2, textureUV), _Normal2Intensity);

                float3 normalblend;
                #ifdef  _MODE_LINEAR
                    normalblend = NormalBlending_Linear(n1, n2);
                    worldNormal = normalize(mul(normalblend, matrix_TBN));
                #elif _MODE_OVERLAY
                    normalblend = NormalBlending_Overlay(n1, n2);
                    worldNormal = normalize(mul(normalblend, matrix_TBN));
                #elif _MODE_PD
                    normalblend = NormalBlending_PartialDerivative(n1, n2, 0.5);
                    worldNormal = normalize(mul(normalblend, matrix_TBN));
                #elif _MODE_WHITEOUT
                    normalblend = NormalBlending_Whiteout(n1, n2);
                    worldNormal = normalize(mul(normalblend, matrix_TBN));
                #elif _MODE_RNM
                    normalblend = NormalBlending_ReorientedNormalMapping(n1, n2);
                    worldNormal = normalize(mul(normalblend, matrix_TBN));
                #else
                    worldNormal = normalize(mul(n1, matrix_TBN));
                #endif

                Light light = GetMainLight();
                float3 lightDir = light.direction;
                float3 lightColor = light.color;

                float lambert = saturate(dot(worldNormal, lightDir));
                float3 finalcol = lambert * lightColor * _BaseColor.xyz;

                return float4(finalcol, 1.0);
            }
            ENDHLSL
        }
    }
}