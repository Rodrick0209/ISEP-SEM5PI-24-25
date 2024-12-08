import { IAllergyCatalogItemPersistence } from "../../dataschema/IAllergyCatalogItemPersistence";
import mongoose from 'mongoose';

const AllergySchema = new mongoose.Schema(
    {
        domainId: { type: String, unique: true, required: true },
        allergyCatalogItem: { type: String} ,
        description: { type: String }
    },
    {
        timestamps: true
    }
);

export default mongoose.model<IAllergyCatalogItemPersistence & mongoose.Document>('Allergy', AllergySchema);