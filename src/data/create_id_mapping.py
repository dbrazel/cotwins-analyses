# Join Robin's paper entry data to the users db table to produce a
# mapping of SVID to the "alternate ID" used in most Michigan
# DB tables

from Crypto.Hash import SHA256
import pandas as pd
import os


# This function comes from Paul Resnick at Michigan
def get_sha256_colorado_id(colorado_id):
    hash = SHA256.new()
    hash.update(colorado_id)
    code = hash.hexdigest()
    return code

paper = pd.read_csv(os.path.join("data", "raw",
                                 "Robin_paper-entry_12-6-16.csv"))
users = pd.read_csv(os.path.join("data", "raw",
                                 "Michigan_DB_users_02_01_17.csv"))

# Create hashed versions of the twin 1 and twin 2 IDs
paper["T1_Hashed"] = paper.T1.map(
    lambda x: get_sha256_colorado_id(x.encode("UTF-8")))
paper["T2_Hashed"] = paper.T2.map(
    lambda x: get_sha256_colorado_id(x.encode("UTF-8")))

# Consecutively join the paper and users tables on the hashed IDs
users = users[["colorado_id", "alternate_id"]]
merged = pd.merge(paper, users, left_on="T1_Hashed",
                  right_on="colorado_id", how="left")
merged = merged.rename(columns={"alternate_id": "T1_alternate_id"})
merged = pd.merge(merged, users, left_on="T2_Hashed",
                  right_on="colorado_id", how="left")
merged = merged.rename(columns={"alternate_id": "T2_alternate_id"})

merged = merged[["T1", "T2", "T1_alternate_id",
                "T2_alternate_id", "bestzygos"]]
merged.to_csv(os.path.join("data", "processed", "id_mapping.csv"), index=False)

# Make a version of the id mapping with each twin on a line
merged1 = merged[["T1", "T1_alternate_id", "bestzygos"]]
merged2 = merged[["T2", "T2_alternate_id", "bestzygos"]]

merged1 = merged1.rename(columns={"T1": "SVID", "T1_alternate_id": "alternate_id"})
merged2 = merged2.rename(columns={"T2": "SVID", "T2_alternate_id": "alternate_id"})

merged = merged1.append(merged2, ignore_index = True)

merged.to_csv(os.path.join("data", "processed", "id_mapping_long.csv"), index=False)
