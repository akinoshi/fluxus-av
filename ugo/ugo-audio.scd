/*
 * File: ugo-audio.scd
 * -------------------
 * A script for audio visual performace called 
 * UGO (Unidentified Green Object).
 *
 * SuperCollider: Control Window (Audio)
 * Fluxus: Main Window (Visual)
 *
 * Author: Akinori Kinoshita
 * E-mail: art.akinoshi -at- gmail.com
 * Date:
 */

(
s.waitForBoot{
	var w;
	var vol;
	var alpha_slide;
	var gain_slide;
	var size_slide;
	var scenes;
	var posX = Array.fill(6, {arg i; i*45+20;});
	var snd_flag;
	var lock_flag;
	var particle;
	var line_flag;

	var melodyA, melodyB, melodyC, melodyD;
	var hihat;
	var ptn1, ptn2, ptn3;

	var grids;

	SynthDef(\ugo, { |freq = 440, amp = 0.5|
		var y;
		var w = 2pi;
		var t = Line.ar(-0.25,1,2.5);
		var tt, a, b, c;
		var env;
		tt = 1 - t;
		a = SinOsc.ar(t*w*0.5)*log(t+0.3)*tt;
		b = SinOsc.ar(t*w)*t*0.4;
		c = tt % 0.075 * SinOsc.ar(tt**3*w)*t*2;
		y = (a+b+c)*tt;
		y = SinOsc.ar(freq) * y ! 2;
		env = EnvGen.kr(Env.perc, 1.0, doneAction: 2);
		Out.ar(0, amp * env * y);
	}).store;

	SynthDef("hihat", {
		arg freq=440, gate=1, amp=1.0, source, pan=0.0;
		source = Pan2.ar(BPF.ar(LFPulse.ar(BPF.ar(WhiteNoise.ar(5), 1200, 0.1).distort * 8000 + 6600, 0.7), 7000,1) * 
				 EnvGen.kr(Env.perc(0.01, 0.4, 1, -6), doneAction: 2), 0);
		Out.ar(0, amp * source);
	}).store;

	SynthDef(\atari2600, {
		arg out=0, gate=1, tone0=5, tone1=8, freq0=10, freq1=20, amp=1, pan=0;
		var e, z;
		e = EnvGen.kr(Env.asr(0.01, amp, 0.05), gate, doneAction:2);
		z = Atari2600.ar(tone0, tone1, freq0, freq1, 15, 15);
		Out.ar(out, Pan2.ar(e * z, pan));
	}).store;

	SynthDef(\space, { |freq = 440, amp = 0.5|
		var y;
		var w = 2pi * freq;
		var t = Line.ar(-0.25,1,2.5);
		var tt, a, b, c;
		var env;
		tt = 1 - t;
		a = SinOsc.ar(t*w*0.5)*log(t+0.3)*tt;
		b = SinOsc.ar(t*w)*t*0.4;
		c = tt % 0.075 * SinOsc.ar(tt**3*w)*t*2;
		y = (a+b+c)*tt;
		y = SinOsc.ar(freq) * y ! 2;
		env = EnvGen.kr(Env.perc, 1.0, doneAction: 2);
		Out.ar(0, Pan2.ar(amp * env * y, 0));
	}).store;

	SynthDef(\rev, {|in= 10, mix= 0.5|
		var z= InFeedback.ar(0, 2);	//read stereo sound from bus number 'in'
			z= FreeVerb.ar(z, mix, 0.8);
		Out.ar(0, z);
	}).store;
	// a = Synth(\rev);

	ptn1 = {
		melodyA = Pbind(
			\instrument, \ugo,
			\midinote, Pseq([55,60,64,60,63,64,60,60,56,55], inf),
			\dur, Pseq([1,2,2,1,6]/4, inf),
			\amp, 0.6
		).play;
	};

	ptn2 = {
		melodyA.stream = Pbind(
			\instrument, \ugo,
			//\midinote, Pseq([55,60], inf),
			\midinote, Prand([Pseq([55,60], 16),
							  Pseq([64,60], 16)], inf),
			\dur, 1/4,
			\amp, 0.6
		).asStream;
	};

	hihat = Pbind(
		\instrument, \hihat,
		\amp, Pseq([0,0.3], inf),
		\dur, 1/2,
		\midinote, Pseq([\,1], inf),
		\dummy, Pfunc({ arg evt;
			~host.sendMsg("/hihat", evt.at(\amp)*10);
			// evt.at(\amp).postln;
		})
	).play;
	hihat.mute;

	melodyB = Pbind(
		\instrument, \atari2600,
		\tone0, 0,
		\freq0, 0,
		\tone1, 10,
		\freq1, Pseq([0,2,2,8,7,2,2,1,0,9], inf),
		\dur, Pseq([1,1,3,2,1,1,1,2,1,1]/4, inf),
		\legato, Pseq([2,4,1,2,2,4,1,3,1,1]/4, inf),
		\amp, 0.3
	).play;
	melodyB.mute;

	melodyC = Pbind(
		\instrument, \space,
		\midinote, Pseq([55,57,62,55,57,60,55,57,62,55,57,60,62,60,62,60], inf),
		\dur, 1/4,
		\amp, Pseq([1,0,0,1,Pwrand([0.5,0],[0.1,0.9],1),0,1,0]*0.3, inf)
	).play;
	melodyC.mute;

	melodyD = Pbind(
		\instrument, \atari2600,
		\tone0, 0,
		\freq0, 0,
		\tone1, 6,
		\freq1,  Pseq([3,1,2,5,8,1,8,0,8,0], inf),
		\dur,    Pseq((1..4).mirror/8, inf),
		\legato, Pseq([6,1,7,8,5,6]/8, inf),
		\amp, 0.4,
		\dummy, Pfunc({ arg evt;
			~host.sendMsg("/melody", evt.at(\freq1));
		})
	).play;
	melodyD.mute;

	// simple approximation to a drink can
	x = { |t_trig=0|
		var strike = EnvGen.ar(Env.perc(0.0001, 0.001, 0.1), t_trig);
		var son = Ringz.ar(strike, [359, 426, 1748, 3150], 0.2).sum;
		son = HPF.ar(son.clip2(0.6), 300);
		son * 0.2 ! 2;
	}.play;

	// Run this line to hit the can!
	o = OSCresponderNode(NetAddr("localhost", nil),'tincan', {
		x.set(\t_trig, 1);
	}).add;

	SynthDef(\grid, { |amp=0, id=0|
		var snd;
		snd = SinOsc.ar(id+60.midicps, id*pi, amp);
		Out.ar(0, snd);
	}).store;

    // Example14.scd
	SynthDef(\av, {|freq= 400, fm= 1, beat= 1, amp= 0, pan= 0, gate= 1|
		var e= EnvGen.ar(Env.asr(0.01, 1, 0.02), gate, doneAction:2);
		var z= SinOsc.ar(freq*SinOsc.ar(0, SinOsc.ar(fm, 0, 2pi), beat), 0, amp);
		Out.ar(0, Pan2.ar(z, pan, e));
	}).store;

    y = Synth(\av);
	// y.set(\amp, 1);

	p = OSCresponderNode(NetAddr("localhost", nil),'grid', {
		|t, r, msg|
		y.set(
			\freq, msg[1].value.linexp(-5, 5, 2000, 200),
			\amp, msg[2].value.linlin(0, 5, 0, 0.5),
			\beat, msg[3].value.abs*10.linlin(0, 10, 0, 10),
			\pan, msg[4].value.linlin(-5, 5, -1, 1)
		);
		[msg[0].value, msg[1].value, msg[2].value, msg[3].value, msg[4].value].postln;
	}).add;

	~host = NetAddr("localhost", 12000);

	w = Window("ugo-audio", Rect(0, 0, 640, 480));

	vol = Slider(w, Rect(32, 20, 20, 100))
		.action_({
			~host.sendMsg("/alpha", alpha_slide.value);
		});

	StaticText(w, Rect(27, 122, 80, 20))
		.string_("VOL");

	alpha_slide = Slider(w, Rect(76, 20, 20, 100))
		.action_({
			~host.sendMsg("/alpha", alpha_slide.value);
		});

	StaticText(w, Rect(20+45, 122, 80, 20))
		.string_("ALPHA");

	gain_slide = Slider(w, Rect(75+46, 20, 20, 100))
		.action_({
			~host.sendMsg("/gain", gain_slide.value/10);
		});

	StaticText(w, Rect(20+45+50+45, 122, 80, 20))
		.string_("SIZE");

	size_slide = Slider(w, Rect(75+46+44, 20, 20, 100))
		.action_({
			~host.sendMsg("/size", size_slide.value);
		});

	StaticText(w, Rect(20+45+50, 122, 80, 20))
		.string_("GAIN");

	scenes = Array.fill(6, {arg i; 
		Button(w, Rect(posX[i], 145, 40, 20))
			.states_([
				["PTN"++(i+1), Color.black, Color.clear],
				["PTN"++(i+1), Color.black, Color.green]
			])
			.action_({
				~host.sendMsg("/scene", i+1);

				switch(i,
					0, { ptn1.value },
					1, { ptn2.value },
					2, { melodyB.unmute; },
					3, { melodyD.unmute; };
				);

				6.do{arg j; scenes[j].value_(0);};
				scenes[i].value_(1);
			});
		});

	snd_flag = Button(w, Rect(20, 170, 40, 20))
		.states_([
			["SND", Color.black, Color.clear],
			["SND", Color.black, Color.green]
		])
		.action_({
			if(snd_flag.value == 1,
				{ Ndef(\snd, { a=SinOsc;
					Limiter.ar(LeakDC.ar(a.ar(0.11,BRF.ar(a.ar(a.ar(0.12).exprange(1,1e4),2pi),
					1/a.ar(0.13).range(1,[99,100])))))/3
					});
				  Ndef(\snd).play; },
				{ Ndef(\snd).clear; });
		});

	lock_flag = Button(w, Rect(20+40+5, 170, 40, 20))
		.states_([
			["LOCK", Color.black, Color.clear],
			["LOCK", Color.black, Color.green]
		])
		.action_({
			if(lock_flag.value == 1,
			   { ~host.sendMsg("/lock", 1);
			   	 hihat.unmute;
			   },
			   { ~host.sendMsg("/lock", 0);
			     // hihat.mute;
			   });
		});

	Button(w, Rect(2*(40+5)+20, 170, 40, 20))
		.states_([
			["RING", Color.black, Color.clear]
		])
		.action_({ 
			~host.sendMsg("/ring", 1);
		});

	Button(w, Rect(2*(40+5)+20, 195, 40, 20))
		.states_([
			["KILL", Color.black, Color.clear]
		])
		.action_({
			~host.sendMsg("/kill", 1);
		});

	Button(w, Rect(2*(40+5)+20, 195+25, 40, 20))
		.states_([
			["PAR", Color.black, Color.clear]
		])
		.action_({
			~host.sendMsg("/particle", 1);
			melodyC.unmute;
		});
		
	line_flag = Button(w, Rect(3*(40+5)+20, 170, 40, 20))
		.states_([
			["LINE", Color.black, Color.clear],
			["LINE", Color.black, Color.green]
		])
		.action_({
			if(line_flag.value == 1,
			   { ~host.sendMsg("/line", 1); },
			   { ~host.sendMsg("/line", 0); });
		});

	w.front;

};
)

(
// This data structure (like a "hashtable" or "associative array" in other languages) 
// maps from a phone key to a pair of frequencies in Hz. 
// We can push these frequencies to a synth.
~tbl = IdentityDictionary[
        $1 -> [[697, 1209]],
        $2 -> [[770, 1209]],
        $3 -> [[852, 1209]],
        $4 -> [[697, 1336]],
        $5 -> [[770, 1336]],
        $6 -> [[852, 1336]],
        $7 -> [[697, 1477]],
        $8 -> [[770, 1477]],
        $9 -> [[852, 1477]],
        $* -> [[697, 1633]],
        $0 -> [[770, 1633]],
        $# -> [[852, 1633]],
        $A -> [[941, 1209]],
        $B -> [[941, 1336]],
        $C -> [[941, 1477]],
        $D -> [[941, 1633]]
];
 
// Here we define a SynthDef which plays a single "number" at a time.
// Note that our strategy here is a bit different from the PD code in the book:
//   there, a single pair of sine-wave oscillators was re-used for each number,
//   whereas here, we create (and later free) an individual synth for each number.
SynthDef(\dtmf, {|freq=#[770, 1633], out=0, amp=0.2, gate=1|
        var son, env;
        son = SinOsc.ar(freq, 0, amp).sum;
        env = EnvGen.ar(Env.asr(0.001, 1, 0.001), gate, doneAction: 2);
        Out.ar(out, Pan2.ar(son * env * amp));
}).memStore;
)
 
// Check that it works:
x = Synth(\dtmf) // create
x.set(\gate, 0)  // free
 
(
// This pattern generates a random "phone number" and dials it
Pbind(
        \instrument, \dtmf,
        \dur, 0.2, // or for more "human" timing, try   Pwhite(0.2, 0.5, inf)
        \sustain, 0.15,
        \amp, 0.3,
        \freq, Prand(~tbl.asArray, 13)
).play;
)
 
(
// You could even dial a specific number:
Pbind(
        \instrument, \dtmf,
        // \dur, 0.2, // or for more "human" timing, try   Pwhite(0.2, 0.5, inf)
		\dur, Pwhite(0.2, 0.5, inf),
        \sustain, 0.15,
        \amp, 0.6,
        \freq, Pseq("19840201".collectAs({|digit| ~tbl[digit] }, Array), inf)
).play;
)

3.do({ arg i; i.postln; })

-3.35.abs.linlin(0, 5, 2000, 20)

Pdef(\foo ,Pbind(
	\instrument, \space,
	\midinote, Pseq([55,57,62,55,57,60,55,57,62,55,57,60,62,60,62,60], inf),
	\dur, 1/4,
	// \amp, Pseq([1,0,0,1,Pwrand([0.5,0],[0.1,0.9],1),0,1,0]*0.3, inf)
	\amp, 0.2
)).play;

Pdef(\bar, Pbind(
	\instrument, \atari2600,
	\tone0, 0,
	\freq0, 0,
	\tone1, 12,
	\freq1,  Pseq([3,1,2,5,8,1,8,0,8,0], inf),
	// \dur,    Pseq((1..4).mirror/8, inf),
	\dur, 1/4,
	// \legato, Pseq([6,1,7,8,5,6]/8, inf),
	\legato, 1,
	\amp, 0.3,
	\dummy, Pfunc({ arg evt;
		~host.sendMsg("/melody", evt.at(\freq1));
	})
)).play;
