import { useEffect } from "react";

const useEffectOnMount = (effect: React.EffectCallback) =>
  useEffect(() => effect());

export default useEffectOnMount;
