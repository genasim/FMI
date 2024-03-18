//  Promises call-chain

// fetch("users.json")
//   .then((resRaw) => resRaw.json())
//   .then((users) => {
//     const requests = users.map((user) =>
//       fetch(`https://api.github.com/users/${user.username}`).then((userRes) =>
//         userRes.json()
//       )
//     );

//     Promise.allSettled(requests)
//       .then((gitUsersResults) => {
//         const gitUsers = gitUsersResults
//           .filter((result) => result.status === "fulfilled")
//           .map((gitUser) => gitUser.value);
//         console.log(gitUsers);

//         // TODO: Show pictures (avatar_url)
//         return gitUsers.map((user) => {
//           const img = new Image();
//           img.src = user.avatar_url;
//           document.getElementById("results").appendChild(img);
//           return img;
//         });
//       })
//       .then((images) =>
//           new Promise((resolve) => setTimeout(resolve, 5000, images))
//       )
//       .then((images) => {
//         images.forEach((img) => img.remove());
//       })
//       .finally(() => console.log("Demo has finished"));
//   });

// Async-await version

(async () => {
  try {
    const resRaw = await fetch("users.json");
    const users = await resRaw.json();

    const requests = users.map(async (user) => {
      const userRes = await fetch(
        `https://api.github.com/users/${user.username}`
      );
      return await userRes.json();
    });

    const gitUsersResults = await Promise.allSettled(requests);
    const gitUsers = gitUsersResults
      .filter((result) => result.status === "fulfilled")
      .map((gitUser) => gitUser.value);

    console.log(gitUsers);

    const images = gitUsers.map((user) => {
      const img = new Image();
      img.src = user.avatar_url;
      document.getElementById("results").appendChild(img);
      return img;
    });

    await new Promise((resolve) => setTimeout(resolve, 5000));
    images.forEach((img) => img.remove());
  } catch (error) {
    console.log("Error: ", error);
  } finally {
    console.log("Demo has finished!");
  }
})();
