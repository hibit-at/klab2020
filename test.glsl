// Distance Function

vec3 trans(vec3 p){
    return mod(p, 3.0) - 1.0;
}

float sphere(vec3 p, float s) {
    return length(trans(p)) - s;
}

float map(vec3 p) {
	float s = sphere(p, 0.5);
    return s;
}

vec3 CreateRay(vec2 p, vec3 cameraPos, vec3 cameraTarget, float fov) {
    vec3 forward = normalize(cameraTarget - cameraPos);
    vec3 side = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = normalize(cross(forward, side));
    return normalize(forward * fov + side * p.x + up * p.y);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (fragCoord.xy * 2.0 - iResolution.xy) / min(iResolution.x, iResolution.y);
    
    //カメラの位置
    vec3 cameraPos = vec3(sin(iTime) * 5.0, 0.0, cos(iTime) * 5.0);
    
    // カメラの注視点
    vec3 cameraTarget = vec3(0.0);
    
    // シェーディングピクセルのカメラからシーンへのレイ
    vec3 ray = CreateRay(p, cameraPos, cameraTarget, 2.5);

	// レイマーチング
    float t = 0.01;
    vec3 col = vec3(0.0);
    for(int i=0; i<99; i++) {
    	vec3 pos = cameraPos + ray * t;
        float d = map(pos);
        if (d < 0.001) {
        	col = vec3(1.0 - float(i) / 70.);
            break;
        }
        t += d;
    }
    fragColor = vec4(col,1.0);
}