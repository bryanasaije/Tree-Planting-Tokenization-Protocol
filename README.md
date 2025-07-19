# 🌳 Tree Planting Tokenization Protocol

A blockchain-based solution for transparent and verifiable tree planting initiatives.

## 🎯 Overview

The Tree Planting Tokenization Protocol enables transparent tracking and verification of tree planting initiatives through NFTs. Each planted tree is represented by a unique token containing vital information such as location, growth metrics, and verification history.

## ✨ Features

- 🌱 Mint NFTs for newly planted trees with GPS coordinates
- 📏 Track tree growth and health metrics
- ✅ Verification system for authorized updates
- 📊 Historical growth data storage
- 🔄 Transferable tree ownership

## 🚀 Smart Contract Functions

### Public Functions

1. `plant-tree`: Mint a new tree NFT with location data
2. `verify-tree-growth`: Update tree metrics through authorized verification
3. `transfer`: Transfer tree ownership
4. `set-verifier`: Update the authorized verifier address

### Read-Only Functions

1. `get-token-uri`: Retrieve token metadata URI
2. `get-owner`: Get current tree owner
3. `get-tree-data`: Retrieve tree details
4. `get-verification-history`: Access historical verification data

## 🛠️ Usage

1. Deploy the contract using Clarinet
2. Plant trees by calling `plant-tree` with valid GPS coordinates
3. Verify growth using an authorized verifier
4. Track progress through the read-only functions

## 🔐 Security

- Only contract owner can set verifiers
- Only authorized verifiers can update tree data
- Coordinate validation ensures proper location format
- Historical data is preserved for transparency

## 📝 License

MIT
```
