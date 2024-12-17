import mongoose from "mongoose";
import { IMedicalConditionCatalogPersistence } from "../../dataschema/IMedicalConditionCatalogPersistence";

const MedicalConditionCatalogSchema = new mongoose.Schema(
    {
        domainId: { type: String, unique: true },
        code: { type: String, unique: true },
        designation: { type: String },
        description: { type: String, default: null },
        commonSymptoms: { type: String, default: null }
    },
    {
        timestamps: true
    }
);

export default mongoose.model<IMedicalConditionCatalogPersistence & mongoose.Document>('MedicalConditionCatalog', MedicalConditionCatalogSchema);