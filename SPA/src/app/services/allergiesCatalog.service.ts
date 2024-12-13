import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { AllergyCatalogItem } from '../models/allergyCatalog';
import { AllergyCatalogMapper } from '../mappers/allergyCatalogMapper';

@Injectable({
  providedIn: 'root'
})
export class AllergyCatalogService {
  private baseUrl = '/api2/allergiesCatalog';
  private getAllergiesUrl = '/getAll';
  private getAllergyUrl = '/get';
  private createUrl = '/create';
  private updateUrl = '/update';

  //private baseUrl = 'http://localhost:4000/api2/allergiesCatalog';

  
  constructor(private http: HttpClient) {}

  getAllergiesFromCatalog(): Observable<AllergyCatalogItem[]> {
    const url = `${this.baseUrl}${this.getAllergiesUrl}`;
    return this.http.get<AllergyCatalogItem[]>(url).pipe(
      map((data: AllergyCatalogItem[]) => AllergyCatalogMapper.mapToAllergyCatalogItems(data))
    );
  }

  createAllergyCatalogItem(name: string): Observable<AllergyCatalogItem> {
    const url = `${this.baseUrl}${this.createUrl}`;
    return this.http.post<AllergyCatalogItem>(url, { name }).pipe(
      map((data: AllergyCatalogItem) => AllergyCatalogMapper.mapToAllergyCatalogItem(data))
    );
  }

  getAllergyCatalogItem(name: string): Observable<AllergyCatalogItem> {
    const url = `${this.baseUrl}${this.getAllergyUrl}/${name}`;
    return this.http.get<AllergyCatalogItem>(url).pipe(
      map((data: AllergyCatalogItem) => AllergyCatalogMapper.mapToAllergyCatalogItem(data))
    );
  }

  updateAllergyCatalogItem(name: string, nameToEdit: string): Observable<AllergyCatalogItem> {
    const url = `${this.baseUrl}${this.updateUrl}/${name}`;
    const body = { nameToEdit: nameToEdit };
    return this.http.put<AllergyCatalogItem>(url, body).pipe(
      map((data: AllergyCatalogItem) => AllergyCatalogMapper.mapToAllergyCatalogItem(data))
    );
  }
 
}