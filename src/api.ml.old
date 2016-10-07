module Make(B : HardCaml.Comb.S) = struct

  module Waveterm_waves = HardCamlWaveTerm.Wave.Make( HardCamlWaveTerm.Wave.Bits(B) )
  module Waveterm_sim = HardCamlWaveTerm.Sim.Make(B)(Waveterm_waves)
  module Waveterm_ui = Ui.Make(B)(Waveterm_waves)

end

include Make(HardCaml.Bits.Comb.IntbitsList)

