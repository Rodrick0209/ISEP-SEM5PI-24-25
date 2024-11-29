import { IAllergyPersistence } from "../../dataschema/IAllergyPersistence";
import mongoose from 'mongoose';

const AllergyCatalogSchema = new mongoose.Schema(
    {
        domainId: { type: String, unique: true },
        name: { type: String, unique: true }
    },
    {
        timestamps: true
    }
    );

export default mongoose.model<IAllergyPersistence & mongoose.Document>('Allergy', AllergyCatalogSchema);