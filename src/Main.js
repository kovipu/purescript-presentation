export const compute = (n) => {
  return n + 4;
};

export const computeWithSideEffects = (n) => {
  const rand = Math.floor(Math.random() * 10);
  return n + rand;
};

export const computeWithSideEffects_ = (n) => () => {
  const rand = Math.floor(Math.random() * 10);
  return n + rand;
};
