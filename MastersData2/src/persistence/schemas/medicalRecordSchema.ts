import { IMedicalRecordPersistence } from "../../dataschema/IMedicalRecordPersistence";
import mongoose from 'mongoose';
import { Allergy } from "../../domain/allergy";
import { MedicalCondition } from "../../domain/medicalCondition";

const MedicalRecordSchema = new mongoose.Schema(
    {
        id: { type: String, unique: true },
        patientId: { type: String, unique: true },
        allergies: { type: [mongoose.Schema.Types.String], ref: 'Allergy'},
        medicalConditions: { type: [mongoose.Schema.Types.String], ref: 'MedicalCondition'}
    },
    {
        timestamps: true
    }
);

export default mongoose.model<IMedicalRecordPersistence & mongoose.Document>('MedicalRecord', MedicalRecordSchema);