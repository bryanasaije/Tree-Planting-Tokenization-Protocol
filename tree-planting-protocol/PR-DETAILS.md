Tree Planting Tokenization Protocol

Overview
Comprehensive smart contract system for tokenizing tree planting activities with carbon credit generation, verification workflows, and seasonal rewards. Enables transparent tracking of reforestation efforts with economic incentives.

Technical Implementation
- **Tree Registry**: Complete NFT-like system with geolocation, species tracking, and growth monitoring
- **Carbon Credits**: Fungible token system for carbon sequestration with automatic issuance upon verification  
- **Verifier Network**: Role-based access control for authorized tree verification agents
- **Seasonal Rewards**: Incentive system with bonuses for verified plantings
- **Trading System**: Peer-to-peer carbon credit exchange functionality

Key Functions:
- `plant-tree`: Register new tree with location and carbon potential
- `verify-tree`: Official verification with automatic carbon credit issuance  
- `trade-carbon-credits`: Transfer credits between users
- `claim-seasonal-reward`: Claim planting bonuses based on verification rates
- `get-contract-stats`: Real-time protocol statistics

Testing & Validation
- ✅ Contract passes clarinet check
- ✅ All npm tests successful  
- ✅ CI/CD pipeline configured
- ✅ Clarity v3 compliant with proper error handling
