
l0 :: HM.HashMap T.Text (HM.HashMap T.Text (HS.HashSet T.Text))
l0 = HM.fromList [
      ("l0a", l10)
    , ("l0b", l11)
    ]

l10 :: HM.HashMap T.Text (HS.HashSet T.Text)
l10 = HM.fromList [
      ("l10a", s10a)
    , ("l10b", s10a)
    ]

l11 :: HM.HashMap T.Text (HS.HashSet T.Text)
l11 = HM.fromList [
      ("l11a", s11a)
    , ("l11b", s11b)
    ]

s10a :: HS.HashSet T.Text
s10a = HS.fromList ["s10a-1", "s10a-2"]

s10b :: HS.HashSet T.Text
s10b = HS.fromList ["s10b-1", "s10b-2"]

s11a :: HS.HashSet T.Text
s11a = HS.fromList ["s11a-1", "s11a-2"]

s11b :: HS.HashSet T.Text
s11b = HS.fromList ["s11b-1", "s11b-2"]

f0 :: [Criterion T.Text]
f0 = [Break "l0a"]

f1 :: [Criterion T.Text]
f1 = [Cont "l0a", Break "l10a"]

f2 :: [Criterion T.Text]
f2 = [Cont "l0a", Cont "l10a", Break "s10a-1"]

r0 :: [Criterion FromTo]
r0 = [Break ("l0a", "L0A")]

r1 :: [Criterion FromTo]
r1 = [Cont ("l(0)a", "L-\\1-A"), Break ("l10a", "L10A")]

r2 :: [Criterion FromTo]
r2 = [Cont ("l0a", "L10A"), Cont ("l10a", "L10A"), Break ("s10a-1", "S10A-1")]
