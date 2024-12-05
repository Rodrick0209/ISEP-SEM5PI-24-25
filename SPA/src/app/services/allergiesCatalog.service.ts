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
  private createUrl = '/create';

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
  
}