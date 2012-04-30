/*
 * File: cell-div-audio.scd
 * ------------------------
 * A script for audio visual performace called 
 * Cell Division. This performance is about
 * transition from DNA double helix to cell
 * division and x chromosome (male/female).
 *
 * SuperCollider: Control Window (Audio)
 * Fluxus: Main Window (Visual)
 *
 * Author: Akinori Kinoshita
 * E-mail: art.akinoshi -at- gmail.com
 * Date: Thu Nov 24 23:04:56 CST 2011
 */

(
s.waitForBoot{
	var w;
	var snake_fade;
	var paths;
	var posX = Array.fill(9, {arg i; i*45+20;});
	var cell_fade;
	var snake, cell, div;
	var gain;
	var blend;
	var speed;
	var par;
	var max_flag;
	var str_1, str_2;
	var face;
	var box_flag;
	var core;
	var test;
	var rev;

	~host = NetAddr("localhost", 12000);

	o = OSCresponderNode(
	NetAddr("localhost", nil),
	'/freq',
	{ |t, r, msg|
		var fa, fb;
		var la, lb;
		la = "fa:" + msg[1].value;
		fa = msg[1].linlin(0, 1, 4000, 200);
		la.postln;
		fb = msg[2].linlin(0, 1, 0, 0.25);
		lb = "fb:" + msg[2].value;
		lb.postln;
		a.set(\freqa, fa);
		a.set(\freqb, fb);
		b.set(\freqa, fa);
		b.set(\freqb, fb);
		a.set(\amp, snake_fade.value);
		b.set(\amp, snake_fade.value);
	}).add;

	SynthDef(\snake,
	{|out=0, amp=0.5, pan=0, freqa=440, freqb=0, mod=7, detune=0|
		var f, a, z;
		f = freqa + detune;
		a = SinOsc.ar(f + (f * SinOsc.ar(mod,0,0.02)), mul: freqb);
		z = Mix.ar(a);
		Out.ar(0, amp*z) + Out.ar(1, amp*z);
	}).store;

	a = Synth(\snake);
	a.set(\mod, 12);

	b = Synth(\snake);
	b.set(\mod, 5, \detune, 200); 

	SynthDef(\camera_pan, 
	{|amp=1,speed=1,revMix=0.5,revRoom=1.5,attack=0.5,pan=0|
		Out.ar(0, Pan2.ar(amp *
		FreeVerb.ar(
		Blip.ar(880, 0 , 0.3)
		+
		BPF.ar(PinkNoise.ar(0.5), 800) * LFSaw.ar(1)　
		+
		LPF.ar(PinkNoise.ar(0.25), 100) * LFSaw.ar(5)
		, revMix , revRoom, 0.8) * 
		EnvGen.ar(Env.perc(attack), doneAction:2) * 4;
		,pan));
	}).store;

	test= NodeProxy.new;
	test.edit;

	test.source= {|pulse= 2, roomsize= 16, revtime= 1, f= 0.02|
	GVerb.ar(
		Splay.ar(
			// Blip or SinOsc
			Blip.ar([440,440*2,440*3,440*4],
				  SinOsc.kr(0,0,pi),
				  LFPulse.ar(pulse,0,SinOsc.ar(f,0,0.25),0.25)),
		0.6,
		1.0,
		FSinOsc.kr(2)
		),
	roomsize,
	revtime)
	};

	Ndef(\background, {
	var n= 5;
	Mix({|i|
	var z= SinOsc.ar(0,
		SinOsc.ar(123*i+(SinOsc.ar(0.01)),2pi,
		VarSaw.ar(0.8*i,SinOsc.ar(8,0,2pi),
		LFTri.ar(160),2pi),SinOsc.ar(0.1)),
		1/n);
	Pan2.ar(z/4, z);}!n)
	});

	Ndef(\background).fadeTime = 20;

	SynthDef(\rev, {|in = 10, mix = 0.5|
		var modulator;
		var z = InFeedback.ar(0, 2);
		modulator = SinOsc.ar(
			MouseX.kr(10,4000,'exponential'),
					  [0,0.5pi]);
		Out.ar(0, z*modulator);
	}).store;

	w = Window("cell-div", Rect(0, 0, 640, 480));

	snake_fade = Slider(w, Rect(31, 20, 20, 100))
		.action_({
			~host.sendMsg("/snake_fade", snake_fade.value);
		});

	StaticText(w, Rect(20, 122, 80, 20))
		.string_("SNAKE");

	cell_fade = Slider(w, Rect(75, 20, 20, 100))
		.action_({
			~host.sendMsg("/cell_fade", cell_fade.value);
		});

	StaticText(w, Rect(20+50, 122, 80, 20))
		.string_("CELL");

	par = Slider(w, Rect(75+46, 20, 20, 100))
		.action_({
			~host.sendMsg("/par", par.value);
		});

	StaticText(w, Rect(20+50+47, 122, 80, 20))
		.string_("PAR");

	gain = Slider(w, Rect(75+44+46, 20, 20, 100))
		.action_({
			~host.sendMsg("/gain", gain.value);
		});

	StaticText(w, Rect(20+50+44+46, 122, 80, 20))
		.string_("GAIN");

	paths = Array.fill(8, {arg i; 
		Button(w, Rect(posX[i], 145, 40, 20))
			.states_([
				["PTN"++(i+1), Color.black, Color.clear],
				["PTN"++(i+1), Color.black, Color.green]
			])
			.action_({
				~host.sendMsg("/path", i+1);
				8.do{arg j; paths[j].value_(0);};
				paths[i].value_(1);
			});
		});
	
	paths[0].value_(1);

	Button(w, Rect(20, 170, 40, 20))
		.states_([
			["左", Color.black, Color.clear]
		])
		.action_({
			~host.sendMsg("/camera", "h");
			Synth(\camera_pan,
				  [\pan, -1,
				   \attack, 0.1,
				   \amp, 0.5]);
		});

	Button(w, Rect(20+40+5, 170, 40, 20))
		.states_([
			["右", Color.black, Color.clear]
		])
		.action_({
			~host.sendMsg("/camera", "l");
			Synth(\camera_pan,
				  [\pan, 1,
				   \attack, 0.1,
				   \amp, 0.5]);
		});

	Button(w, Rect(20+40+5+40+5, 170, 40, 20))
		.states_([
			["AUTO", Color.black, Color.clear],
			["AUTO", Color.black, Color.green]
		])
		.action_({
			~host.sendMsg("/camera", "a");
		});

	snake = Button(w, Rect(20, 4*(20+5)+195, 40, 20))
		.states_([
			["ON", Color.black, Color.green],
			["OFF", Color.black, Color.red]
		])
		.action_({
			if(snake.value == 1,
			   {~host.sendMsg("/snake", 0)},
			   {~host.sendMsg("/snake", 1)});
		});

	cell = Button(w, Rect(20+40+5, 4*(20+5)+195, 40, 20))
		.states_([
			["ON", Color.black, Color.green],
			["OFF", Color.black, Color.red]
		])
		.action_({
			if(cell.value == 1,
			   {~host.sendMsg("/cell", 0)},
			   {~host.sendMsg("/cell", 1)});
		});

	cell.value_(1);

	face = Button(w, Rect(2*(40+5)+20, 4*(20+5)+195, 40, 20))
		.states_([
			["ON", Color.black, Color.green],
			["OFF", Color.black, Color.red]
		])
		.action_({
			if(face.value == 1,
			   {~host.sendMsg("/face", 0)},
			   {~host.sendMsg("/face", 1);
			   	Ndef(\background).fadeTime = 20;
				Ndef(\background).play;});
		});

	face.value_(1);

	core = Button(w, Rect(3*(40+5)+20, 4*(20+5)+195, 40, 20))
		.states_([
			["ON", Color.black, Color.green],
			["OFF", Color.black, Color.red]
		])
		.action_({
			if(core.value == 1,
			   {~host.sendMsg("/core", 0)},
			   {~host.sendMsg("/core", 1);

Ndef(\background, {var n= 33; Mix({|i|
var t= i/n*2pi;
Pan2.ar(LeakDC.ar(SinOsc.ar(Pulse.ar(0.0625, 2/3).range(0.0625, 0.125), 
SinOsc.ar(Pulse.ar(0.1, Pulse.ar(i+1*0.125, 0.4, 1/3, 0.5), i+1*n, i+1*n*2), t, 
SinOsc.ar(SinOsc.ar(0.0625, t), t, 2pi)), 
SinOsc.ar(i+1*0.125, t, i.linexp(0, n-1, 3/n, 0.001)))),
SinOsc.ar(0.125, t, i.linlin(0, n-1, 0, 0.95))
)}!n)});
			   
			   });
		});

	core.value_(1);

	max_flag = Button(w, Rect(20+40+5, 2*(20+5)+195, 40, 20))
		.states_([
			["MAX", Color.black, Color.clear],
			["MAX", Color.black, Color.green]
		])
		.action_({
			if(max_flag.value == 1,
			   {~host.sendMsg("/max", 1)},
			   {~host.sendMsg("/max", 0)});
		});

	box_flag = Button(w, Rect(2*(40+5)+20, 2*(20+5)+195, 40, 20))
		.states_([
			["BOX", Color.black, Color.clear],
			["BOX", Color.black, Color.green]
		])
		.action_({
			if(box_flag.value == 1,
			   {~host.sendMsg("/box", 1);


	Ndef(\background, {
		var n= 5;	//-- increase gradually 1..5
		Mix({|i|
		var z= SinOsc.ar(Pulse.ar(i*2.25,2/3)+
						 Pulse.ar(i*3.25,2/3),
	      	   SinOsc.ar(0,Pulse.ar(20),0,
			   			 Pulse.ar(0.8),i*pi,i/n),
						 			       // 100 or 10
		  	   SinOsc.ar(28.midicps,SinOsc.ar(100*i))*
			   			 Pulse.ar(20/(i+1)/n),
		  	   0.2/n);
			   Pan2.ar(RLPF.ar(z/4, 440));}!n)
		});
			   
			   },
			   {~host.sendMsg("/box", 0)});
		});

	Button(w, Rect(3*(40+5)+20, 2*(20+5)+195, 40, 20))
		.states_([
			["FACE", Color.black, Color.clear]
		])
		.action_({

	var rnd = rrand(0.1, 2.0);
	Ndef(\background).fadeTime = 5;

	Ndef(\background, {
	var n= 5;
	Mix({|i|
	var z= SinOsc.ar(0,
		SinOsc.ar(123*i+(SinOsc.ar(0.01)),2pi,
		VarSaw.ar(rnd*i,SinOsc.ar(8,0,2pi),
		LFTri.ar(160),2pi),SinOsc.ar(0.1)),
		1/n);
	Pan2.ar(z/4, z);}!n)
	});
			
		});

	Button(w, Rect(4*(40+5)+20, 2*(20+5)+195, 40, 20))
		.states_([
			["CORE", Color.black, Color.clear]
		])
		.action_({

Ndef(\background).fadeTime = 0;

Ndef(\background, {var n= 33; Mix({|i|
var t= i/n*2pi;
Pan2.ar(LeakDC.ar(SinOsc.ar(Pulse.ar(0.0625, 2/3).range(0.0625, 0.125), 
SinOsc.ar(Pulse.ar(1, Pulse.ar(i+1*0.125, 0.4, 1/3, 0.5), i+1*n, i+1*n*2), t, 
SinOsc.ar(SinOsc.ar(0.0625, t), t, 2pi)), 
SinOsc.ar(i+1*0.125, t, i.linexp(0, n-1, 3/n, 0.001)))),
SinOsc.ar(0.125, t, i.linlin(0, n-1, 0, 0.95))
)}!n)});
			
		});

	Button(w, Rect(5*(40+5)+20, 2*(20+5)+195, 40, 20))
		.states_([
			["ECHO", Color.black, Color.clear],
			["ECHO", Color.black, Color.green]
		])
		.action_({ arg butt;
			if(butt.value == 1,
			   { rev = Synth(\rev); },
			   { rev.free; });
			~host.sendMsg("/echo", butt.value);
		});

	Button(w, Rect(6*(40+5)+20, 2*(20+5)+195, 40, 20))
		.states_([
			["POLY", Color.black, Color.clear],
			["POLY", Color.black, Color.green]
		])
		.action_({ arg butt;
			if(butt.value == 1,
			   { ~host.sendMsg("/polygon", 1);

Pdef(\seq,
Pbind(
\instrument, \syn2,
\degree, Pseq([1,9,8,4,0,2,0,1].pyramid, inf)+
		 Pseq([2,0,1,1,1,1,1,2].pyramid, inf),
\amp, 1/8,
\dur, 0.125,
\dummy, Pfunc({arg evt;
	~host.sendMsg("/seq", evt.at(\degree));
})
)).play;

			   },
			   { ~host.sendMsg("/polygon", 0); 
				 Pdef(\seq).clear;
				 Ndef(\background).fadeTime = 5;
				 Ndef(\background).clear;
			   });
		});

	Button(w, Rect(4*(40+5)+20, 3*(20+5)+195, 40, 20))
		.states_([
			["SIZE", Color.black, Color.clear]
		])
		.action_({
{ 
	EnvGen.kr(Env.perc(1,0.01,1,4),1,doneAction:2) * 
	// LFTri.ar(XLine.kr(440*10,440,1),0,0.25)!2
	LFTri.ar(440,0,0.25)!2
}.play;
			~host.sendMsg("/shrink", 0);
		});

	Button(w, Rect(5*(40+5)+20, 3*(20+5)+195, 40, 20))
		.states_([
			["FREE", Color.black, Color.clear]
		])
		.action_({
			~host.sendMsg("/free", 0);
		});

	Button(w, Rect(6*(40+5)+20, 3*(20+5)+195, 40, 20))
		.states_([
			["ALL", Color.black, Color.clear]
		])
		.action_({
			~host.sendMsg("/all", 0);
		});

	Button(w, Rect(7*(40+5)+20, 3*(20+5)+195, 40, 20))
		.states_([
			["DRUM", Color.black, Color.clear],
			["DRUM", Color.black, Color.green]
		])
		.action_({ arg butt;
			if(butt.value == 1,
			{ Pdef(\drum,
			  Pbind(
			  \instrument, \drum3,
			  \amp, Pseq([1,1,0,1]/4, inf),
			  \dur, 0.125
			  )).play
			},
			{ Pdef(\drum).clear });
		});

	blend = Array.fill(5, {arg i; 
		Button(w, Rect(posX[i], 1*(20+5)+195, 40, 20))
			.states_([
				["PTN"++(i+1), Color.black, Color.clear],
				["PTN"++(i+1), Color.black, Color.green]
			])
			.action_({
				~host.sendMsg("/blend", i+1);
				5.do{arg j; blend[j].value_(0);};
				blend[i].value_(1);
			});
		});

	speed = NumberBox(w, Rect(20, 195, 40, 20))
		.value_(1)
		.step_(1)
		.clipLo_(0)
		.action_({
			~host.sendMsg("/speed", speed.value);
		});

	div = Button(w, Rect(20+40+5, 195, 40, 20))
		.states_([
			["1", Color.black, Color.clear],
			["2", Color.black, Color.clear]
		])
		.action_({
			if(div.value == 0,
			   {~host.sendMsg("/div", 0)},
			   {~host.sendMsg("/div", 1)});
		});

	str_1 = NumberBox(w, Rect(2*(5+40)+20, 195, 40, 20))
		.value_(0)
		.step_(1)
		.clipLo_(0)
		.action_({
			~host.sendMsg("/str_1", str_1.value);
		});

	str_2 = NumberBox(w, Rect(3*(5+40)+20, 195, 40, 20))
		.value_(0)
		.step_(1)
		.clipLo_(0)
		.action_({
			~host.sendMsg("/str_2", str_2.value);
		});

	w.front;
};
)

10.do{arg i; i.postln;};
String

SynthDef("train_noiseA", {arg amp=1, speed = 1, revMix = 0.5, revRoom =1.5, attack =0.5,pan=0;
Out.ar(0, Pan2.ar(amp *
FreeVerb.ar(
Blip.ar(880, 0 , 0.3)
+
BPF.ar(PinkNoise.ar(0.5), 800) * LFSaw.ar(1)　
+
LPF.ar(PinkNoise.ar(0.25), 100) * LFSaw.ar(5)

, revMix , revRoom, 0.8)* EnvGen.ar(Env.perc(attack), doneAction:2) * 4;
,pan));
}).store;

Synth("train_noiseA", [\pan,1, \attack, 0.7,\amp, 0.7]);
Synth("train_noiseA", [\pan,-1,\attack,0.1+ (0.1.rand),\amp,0.4+0.1.rand]);
Synth("train_noiseA", [\pan,1, \attack, 0.01,\amp,0.5]);

Pdef(\foo,
Pbind(
\instrument, \syn2,
\degree, Pseq([1,9,8,4,0,2,0,1].pyramid, inf)+
		 Pseq([2,0,1,1,1,1,1,2].pyramid, inf),
\amp, 1/8,
\dur, 0.125
)).play
Pdef(\foo).clear;

Pdef(\bar,
Pbind(
\instrument, \drum3,
\amp, Pseq([1,1,0,1]/4, inf),
\dur, 0.125
)).play
Pdef(\bar).clear;

{ CombL.ar(Decay.ar(Dust.ar(1,0.5), 0.2, PinkNoise.ar), 0.1, 0.2, 0.5) }.play;

{ CombL.ar(SinOsc.ar(XLine.kr(20,2000)), 0.2, 0.2, 3) }.play

(
{
var input, modulator;
input = AudioIn.ar([1,2]);
modulator = SinOsc.ar(
	MouseX.kr(10,4000,'exponential'),
		      [0,0.5pi]);
input * modulator;
}.play;
)
