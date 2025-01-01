import dotenv from 'dotenv';
import path from 'path';

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

const envFound = dotenv.config();
if (!envFound) {
  // This error should crash whole process

  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}

export default {
  /**
   * Your favorite port : optional change to 4000 by JRT
   */
  port: parseInt(process.env.PORT, 10) || 2226,
  

  /**
   * That long string from mlab
   */
  // should save it in a .env file
  databaseURL:
    'mongodb+srv://rodrigocastro2004:projetolaprsem5@sem5g55isep.gtdw4.mongodb.net/test?retryWrites=true&w=majority&appName=sem5g55isep',

  /**
   * Your secret sauce
   */
  jwtSecret: 'AMinhaChaveSecretaComMinimo16Caracteres',
  jwtIssuer: 'MyApplication', // Adicione o issuer configurado no backend 1
  jwtAudience: 'MyApplication', // Adicione o audience configurado no backend 1

  /**
   * Used by winston logger
   */
  logs: {
    level: process.env.LOG_LEVEL || 'info',
  },

  /**
   * API configs
   */
  api: {
    prefix: '/api2',
  },

  controllers: {
    role: {
      name: 'RoleController',
      path: '../controllers/roleController',
    },
    allergyCatalog: {
      name: 'AllergyCatalogController',
      path: '../controllers/allergyCatalogController',
    },
    medicalCondition: {
      name: 'MedicalConditionController',
      path: '../controllers/medicalConditionController',
    },
    medicalRecord: {
      name: 'MedicalRecordController',
      path: '../controllers/medicalRecordController',
    },
  },

  repos: {
    role: {
      name: 'RoleRepo',
      path: '../repos/roleRepo',
    },
    user: {
      name: 'UserRepo',
      path: '../repos/userRepo',
    },
    allergyCatalog: {
      name: 'AllergyCatalogRepo',
      path: '../repos/allergyCatalogRepo',
    },
    medicalCondition: {
      name: 'MedicalConditionRepo',
      path: '../repos/medicalConditionRepo',
    },
    medicalRecord: {
      name: 'MedicalRecordRepo',
      path: '../repos/medicalRecordRepo',
    }
  },

  services: {
    role: {
      name: 'RoleService',
      path: '../services/roleService',
    },
    allergyCatalog: {
      name: 'AllergyCatalogService',
      path: '../services/allergyCatalogService',
    },
    medicalCondition: {
      name: 'MedicalConditionService',
      path: '../services/medicalConditionService',
    },
    medicalRecord: {
      name: 'MedicalRecordService',
      path: '../services/medicalRecordService',
    },
  },
};
