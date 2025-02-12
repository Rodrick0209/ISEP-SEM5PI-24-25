import { IMedicalRecordPersistence } from "../../dataschema/IMedicalRecordPersistence";
import mongoose from 'mongoose';
import { Allergy } from "../../domain/allergy";
import allergySchema from "./allergySchema";

const MedicalRecordSchema = new mongoose.Schema(
    {
        id: { type: String, unique: true },
        patientId: { type: String, unique: true },
        allergies: [
            {
                id: { type: String },
                code: { type: String },
                designation: { type: String },
                description: { type: String },
            }
        ],
        medicalConditions: [
            {
                id: { type: String },
                code: { type: String },
                designation: { type: String },
                date: { type: Date }
            }
        ]
    },
    {
        timestamps: true
    }
);


export default mongoose.model<IMedicalRecordPersistence & mongoose.Document>('MedicalRecord', MedicalRecordSchema);