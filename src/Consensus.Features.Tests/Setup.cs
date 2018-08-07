using NUnit.Framework;

namespace Consensus.Features.Tests {

#if DEBUG

    [SetUpFixture]
    class SetupFixture {
        [OneTimeSetUp]
        public void Setup()
        {
            Infrastructure.ZFStar.unitTesting = true;
        }

        [OneTimeTearDown]
        public void TearDown()
        {
            Infrastructure.ZFStar.unitTesting = false;
        }

    }

#endif

}