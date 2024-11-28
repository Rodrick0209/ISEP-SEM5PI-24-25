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
  port: parseInt(process.env.PORT, 10) || 4000, 

  /**
   * That long string from mlab
   */
//  databaseURL: process.env.MONGODB_URI || "mongodb://127.0.0.1:27017/test",
    // should save it in a .env file
    databaseURL: "mongodb+srv://rodrigocastro2004:projetolaprsem5@sem5g55isep.gtdw4.mongodb.net/?retryWrites=true&w=majority&appName=sem5g55isep",

  /**
   * Your secret sauce
   */
  jwtSecret: process.env.JWT_SECRET || "my sakdfho2390asjod$%jl)!sdjas0i secret",

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
    prefix: '/api',
  },

  controllers: {
    role: {
      name: "RoleController",
      path: "../controllers/roleController"
    },
    allergy: {
      name: "AllergyController",
      path: "../controllers/allergyController"
    }
  },

  repos: {
    role: {
      name: "RoleRepo",
      path: "../repos/roleRepo"
    },
    user: {
      name: "UserRepo",
      path: "../repos/userRepo"
    },
    allergy: {
      name: "AllergyRepo",
      path: "../repos/allergyRepo"
    }
  },

  services: {
    role: {
      name: "RoleService",
      path: "../services/roleService"
    },
    allergy: {
        name: "AllergyService",
        path: "../services/allergyService"
    }
  }
};
